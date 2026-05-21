;;; grove-graph-fa2.el --- ForceAtlas2 pure-elisp background-cached engine -*- lexical-binding: t -*-

(require 'grove-core)
(require 'json)

(declare-function grove-graph--update-display "grove-graph")
(declare-function grove-graph--smil-start "grove-graph")
(defvar grove-graph--raw-svg)
(defvar grove-graph--playback-buffer)
(defvar grove-graph--frame-offsets)
(defvar grove-graph--current-frame)

;;; Physics Tuning Constants
(defconst grove-graph-fa2--substeps 10)
(defconst grove-graph-fa2--k-r 50.0)       
(defconst grove-graph-fa2--k-g 0.005)      
(defconst grove-graph-fa2--k-a 0.005)      
(defconst grove-graph-fa2--target-dist 50.0) 
(defconst grove-graph-fa2--friction 0.98)  
(defconst grove-graph-fa2--time-step 0.05) 
(defconst grove-graph-fa2--max-speed 50.0) 
(defconst grove-graph-fa2--canvas-size 500.0)

(defvar grove-graph-fa2--bg-timer nil)
(defvar grove-graph-fa2--bg-buffer nil)
(defvar grove-graph-fa2--bg-frame 0)
(defvar grove-graph-fa2--bg-nodes nil)
(defvar grove-graph-fa2--bg-edges nil)

;; Fast accessors
(defsubst fa2-name (n) (aref n 0))
(defsubst fa2-x (n) (aref n 1))
(defsubst fa2-y (n) (aref n 2))
(defsubst fa2-dx (n) (aref n 3))
(defsubst fa2-dy (n) (aref n 4))
(defsubst fa2-mass (n) (aref n 5))

(defsubst fa2-set-x (n v) (aset n 1 v))
(defsubst fa2-set-y (n v) (aset n 2 v))
(defsubst fa2-set-dx (n v) (aset n 3 v))
(defsubst fa2-set-dy (n v) (aset n 4 v))

(defun grove-graph-fa2--escape-xml (str)
  (let ((s (replace-regexp-in-string "&" "&amp;" str)))
    (setq s (replace-regexp-in-string "<" "&lt;" s))
    (setq s (replace-regexp-in-string ">" "&gt;" s))
    (setq s (replace-regexp-in-string "\"" "&quot;" s))
    s))

(defun grove-graph-fa2--init-sim (adjacency)
  "Initialise node arrays and edge tuples for the physics engine."
  (let ((node-list nil)
        (name-to-idx (make-hash-table :test #'equal))
        (idx 0))
    (dolist (entry adjacency)
      (let ((source (car entry)) 
            (targets (cdr entry)))
        (unless (gethash source name-to-idx)
          (puthash source idx name-to-idx)
          (push (vector source (- (random 1000) 500.0) (- (random 1000) 500.0) 0.0 0.0 (+ 1.0 (length targets))) node-list)
          (cl-incf idx))
        (dolist (target targets)
          (unless (gethash target name-to-idx)
            (puthash target idx name-to-idx)
            (push (vector target (- (random 1000) 500.0) (- (random 1000) 500.0) 0.0 0.0 2.0) node-list)
            (cl-incf idx)))))
    
    (setq grove-graph-fa2--bg-nodes (vconcat (nreverse node-list)))
    
    (let (edge-list)
      (dolist (entry adjacency)
        (let ((source (car entry)) 
              (targets (cdr entry)))
          (when (gethash source name-to-idx)
            (let ((s-idx (gethash source name-to-idx)))
              (dolist (target targets)
                (when (gethash target name-to-idx)
                  (push (cons s-idx (gethash target name-to-idx)) edge-list)))))))
      (setq grove-graph-fa2--bg-edges edge-list))))

(defun grove-graph-fa2--sim-step (cache-file hash-file target-hash target-buf max-frames)
  "Calculates physics with sub-stepping, simulated annealing, and asymptotic velocity compression."
  (let* ((nodes grove-graph-fa2--bg-nodes)
         (edges grove-graph-fa2--bg-edges)
         (len (length nodes))
         (alpha (max 0.01 (- 1.0 (/ (float grove-graph-fa2--bg-frame) max-frames)))))

    ;; PHASE 1: Sub-stepping Physics Loop
    (dotimes (_ grove-graph-fa2--substeps)
      
      ;; Repulsion
      (dotimes (i len)
        (let ((ni (aref nodes i)))
          (cl-loop for j from (1+ i) below len do
                   (let* ((nj (aref nodes j))
                          (dx (- (fa2-x ni) (fa2-x nj)))
                          (dy (- (fa2-y ni) (fa2-y nj)))
                          (dist-sq (max 10.0 (+ (* dx dx) (* dy dy))))
                          (dist (sqrt dist-sq))
                          (force (* alpha (/ (* grove-graph-fa2--k-r (fa2-mass ni) (fa2-mass nj)) dist-sq))))
                     (fa2-set-dx ni (+ (fa2-dx ni) (* (/ dx dist) force)))
                     (fa2-set-dy ni (+ (fa2-dy ni) (* (/ dy dist) force)))
                     (fa2-set-dx nj (- (fa2-dx nj) (* (/ dx dist) force)))
                     (fa2-set-dy nj (- (fa2-dy nj) (* (/ dy dist) force)))))))

      ;; Attraction
      (dolist (edge edges)
        (let* ((u (aref nodes (car edge)))
               (v (aref nodes (cdr edge)))
               (dx (- (fa2-x u) (fa2-x v)))
               (dy (- (fa2-y u) (fa2-y v)))
               (dist (max 0.1 (sqrt (+ (* dx dx) (* dy dy)))))
               (force (* alpha grove-graph-fa2--k-a (- dist grove-graph-fa2--target-dist))))
          (fa2-set-dx u (- (fa2-dx u) (* (/ dx dist) force)))
          (fa2-set-dy u (- (fa2-dy u) (* (/ dy dist) force)))
          (fa2-set-dx v (+ (fa2-dx v) (* (/ dx dist) force)))
          (fa2-set-dy v (+ (fa2-dy v) (* (/ dy dist) force)))))

      ;; Integration (Gravity, Friction, and Position Update)
      (dotimes (i len)
        (let* ((n (aref nodes i))
               (dist (max 0.1 (sqrt (+ (* (fa2-x n) (fa2-x n)) (* (fa2-y n) (fa2-y n))))))
               (grav-force (* alpha grove-graph-fa2--k-g (fa2-mass n))))
          (fa2-set-dx n (- (fa2-dx n) (* (/ (fa2-x n) dist) grav-force)))
          (fa2-set-dy n (- (fa2-dy n) (* (/ (fa2-y n) dist) grav-force)))
          
          ;; Asymptotic Velocity Compression 
          (let ((speed (sqrt (+ (* (fa2-dx n) (fa2-dx n)) (* (fa2-dy n) (fa2-dy n))))))
            (when (> speed 0.1)
              (let* ((v-max grove-graph-fa2--max-speed)
                     (compressed-speed (/ (* speed v-max) (+ speed v-max)))
                     (scale (/ compressed-speed speed)))
                (fa2-set-dx n (* (fa2-dx n) scale))
                (fa2-set-dy n (* (fa2-dy n) scale)))))
          
          ;; Throttled Position Update
          (fa2-set-x n (+ (fa2-x n) (* (fa2-dx n) grove-graph-fa2--time-step)))
          (fa2-set-y n (+ (fa2-y n) (* (fa2-dy n) grove-graph-fa2--time-step)))
          
          (fa2-set-dx n (* (fa2-dx n) grove-graph-fa2--friction))
          (fa2-set-dy n (* (fa2-dy n) grove-graph-fa2--friction)))))

    ;; PHASE 2: SVG Rendering
    (with-current-buffer grove-graph-fa2--bg-buffer
      (insert (format "<svg viewBox=\"-%.1f -%.1f %.1f %.1f\" xmlns=\"http://www.w3.org/2000/svg\">\n" 
                      (/ grove-graph-fa2--canvas-size 2) 
                      (/ grove-graph-fa2--canvas-size 2) 
                      grove-graph-fa2--canvas-size 
                      grove-graph-fa2--canvas-size))
      
      (dolist (edge edges)
        (let ((u (aref nodes (car edge))) 
              (v (aref nodes (cdr edge))))
          (insert (format "  <line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"#585b70\" stroke-width=\"2\" />\n" 
                          (fa2-x u) (fa2-y u) (fa2-x v) (fa2-y v)))))
      
      (dotimes (i len)
        (let ((n (aref nodes i)))
          (insert (format "  <circle cx=\"%.2f\" cy=\"%.2f\" r=\"10\" fill=\"#89b4fa\" />\n  <text x=\"%.2f\" y=\"%.2f\" fill=\"#cdd6f4\" font-size=\"10\" text-anchor=\"middle\">%s</text>\n" 
                          (fa2-x n) (fa2-y n) (fa2-x n) (- (fa2-y n) 15.0) 
                          (grove-graph-fa2--escape-xml (fa2-name n))))))
      
      (insert "</svg>\n<FRAME_SPLIT>\n"))

    ;; PHASE 3: Loop Control
    (if (>= grove-graph-fa2--bg-frame (1- max-frames))
        (progn
          (with-current-buffer grove-graph-fa2--bg-buffer
            (let ((coding-system-for-write 'utf-8)) 
              (write-region (point-min) (point-max) cache-file nil 'silent)))
          (with-temp-file hash-file (insert target-hash))
          (kill-buffer grove-graph-fa2--bg-buffer)
          (message "Background cache render complete.")
          (when (buffer-live-p target-buf) 
            (grove-graph-fa2--load-and-play target-buf cache-file)))
      
      (cl-incf grove-graph-fa2--bg-frame)
      (setq grove-graph-fa2--bg-timer (run-with-timer 0.01 nil #'grove-graph-fa2--sim-step cache-file hash-file target-hash target-buf max-frames)))))

(defun grove-graph-fa2--load-and-play (buf cache-file)
  "Streams the fully computed cache file back to the Emacs frontend."
  (with-current-buffer buf
    (let ((playback-buf (generate-new-buffer " *grove-fa2-playback*")) 
          (offsets nil))
      (with-current-buffer playback-buf
        (let ((coding-system-for-read 'utf-8)) 
          (insert-file-contents-literally cache-file))
        (goto-char (point-min))
        (let ((start (point)))
          (while (search-forward "<FRAME_SPLIT>\n" nil t)
            (push (cons start (match-beginning 0)) offsets)
            (when (looking-at "\n") (forward-char 1))
            (setq start (point)))
          (when (< start (point-max)) 
            (push (cons start (point-max)) offsets))))
      
      (let* ((offsets-vec (vconcat (nreverse offsets))) 
             (first-bounds (aref offsets-vec 0)))
        (setq-local grove-graph--playback-buffer playback-buf)
        (setq-local grove-graph--frame-offsets offsets-vec)
        (setq-local grove-graph--current-frame 0)
        (setq-local grove-graph--raw-svg (with-current-buffer playback-buf 
                                           (buffer-substring-no-properties (car first-bounds) (cdr first-bounds))))
        (grove-graph--update-display)
        (message "Graph playback started.")
        (grove-graph--smil-start)))))

(defun grove-graph-fa2-start (buf adjacency)
  "Initialise the physics background worker or load from cache."
  (let* ((cache-dir (expand-file-name ".cache" grove-directory))
         (hash-file (expand-file-name "fa2-graph.hash" cache-dir))
         (data-file (expand-file-name "fa2-graph.dat" cache-dir)))
    
    (unless (listp adjacency)
      (error "Adjacency is not a list: %S" adjacency))
    
    (let* ((json-payload (json-encode adjacency))
           (current-hash (secure-hash 'md5 json-payload))
           (cached-hash (when (file-exists-p hash-file) 
                          (with-temp-buffer 
                            (insert-file-contents hash-file) 
                            (string-trim (buffer-string))))))
      
      (unless (file-exists-p cache-dir) 
        (make-directory cache-dir t))
      
      (if (and cached-hash (string= current-hash cached-hash) (file-exists-p data-file))
          (progn
            (message "Loading cached graph...")
            (grove-graph-fa2--load-and-play buf data-file))
        
        (message "Rendering cache in background (this may take a few minutes)...")
        (when grove-graph-fa2--bg-timer (cancel-timer grove-graph-fa2--bg-timer))
        (when (buffer-live-p grove-graph-fa2--bg-buffer) (kill-buffer grove-graph-fa2--bg-buffer))
        
        (setq grove-graph-fa2--bg-frame 0)
        (setq grove-graph-fa2--bg-buffer (generate-new-buffer " *grove-fa2-bg*"))
        (grove-graph-fa2--init-sim adjacency)
        (setq grove-graph-fa2--bg-timer (run-with-timer 0.01 nil #'grove-graph-fa2--sim-step data-file hash-file current-hash buf 840))))))

;;;###autoload
(defun grove-graph-fa2-clear-cache ()
  "Clears the background render cache to force a fresh physics simulation."
  (interactive)
  (let* ((cache-dir (expand-file-name ".cache" grove-directory))
         (hash-file (expand-file-name "fa2-graph.hash" cache-dir))
         (data-file (expand-file-name "fa2-graph.dat" cache-dir)))
    (when (file-exists-p hash-file) (delete-file hash-file))
    (when (file-exists-p data-file) (delete-file data-file))
    (message "ForceAtlas2 cache cleared. Run grove-graph to regenerate.")))

(provide 'grove-graph-fa2)
