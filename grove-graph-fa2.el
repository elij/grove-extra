;;; grove-graph-fa2.el --- ForceAtlas2 pure-elisp background-cached engine -*- lexical-binding: t -*-

(eval-when-compile
  (when (boundp 'comp-speed)
    (setq comp-speed 3)))

(require 'cl-lib)
(require 'grove-core)
(require 'json)

(declare-function grove-graph--smil-start "grove-graph")
(declare-function grove-graph--update-display "grove-graph")

(defconst grove-graph-fa2--substeps 10)
(defconst grove-graph-fa2--k-r 50.0)       
(defconst grove-graph-fa2--k-g 0.005)      
(defconst grove-graph-fa2--k-a 0.005)      
(defconst grove-graph-fa2--target-dist 50.0) 
(defconst grove-graph-fa2--friction 0.98)  
(defconst grove-graph-fa2--time-step 0.05) 
(defconst grove-graph-fa2--max-speed 50.0)
(defconst grove-graph-fa2--canvas-size 500.0)
(defconst grove-graph-fa2--event-horizon 240.0)

(defvar grove-graph--current-frame)
(defvar grove-graph--frame-offsets)
(defvar grove-graph--playback-buffer)
(defvar grove-graph--raw-svg)

(defvar grove-graph-fa2--bg-buffer nil)
(defvar grove-graph-fa2--bg-edges nil)
(defvar grove-graph-fa2--bg-frame 0)
(defvar grove-graph-fa2--bg-nodes nil)
(defvar grove-graph-fa2--bg-timer nil)
(defvar grove-graph-fa2--frames-rendered 0)
(defvar grove-graph-fa2--heavy-frames 0)
(defvar grove-graph-fa2--heavy-time 0.0)
(defvar grove-graph-fa2--mass-matrix nil)
(defvar grove-graph-fa2--playback-started nil)
(defvar grove-graph-fa2--start-time nil)
(defvar grove-graph-fa2-playback-penalty 0.020)

(defsubst fa2-color (n) (aref n 6))
(defsubst fa2-dx (n) (aref n 3))
(defsubst fa2-dy (n) (aref n 4))
(defsubst fa2-mass (n) (aref n 5))
(defsubst fa2-name (n) (aref n 0))
(defsubst fa2-set-dx (n v) (aset n 3 v))
(defsubst fa2-set-dy (n v) (aset n 4 v))
(defsubst fa2-set-x (n v) (aset n 1 v))
(defsubst fa2-set-y (n v) (aset n 2 v))
(defsubst fa2-x (n) (aref n 1))
(defsubst fa2-y (n) (aref n 2))

(defun grove-graph-fa2--escape-xml (str)
  (let ((s (replace-regexp-in-string "&" "&amp;" str)))
    (setq s (replace-regexp-in-string "<" "&lt;" s))
    (setq s (replace-regexp-in-string ">" "&gt;" s))
    (setq s (replace-regexp-in-string "\"" "&quot;" s))
    s))

(defun grove-graph-fa2--hash-pos (str offset)
  "Return a pseudo-random number between -500 and 500 based on STR and OFFSET or random."
  (if grove-graph-deterministic-positions
      (- (mod (string-to-number (substring (secure-hash 'md5 (concat str offset)) 0 8) 16) 1000) 500.0)
    (- (random 1000.0) 500.0)))

(defun grove-graph-fa2--node-color (title)
  "Determine the node colour based on its tags and `grove-graph-tag-groups'."
  (let ((color "#89b4fa"))
    (when (boundp 'grove-graph-tag-groups)
      (catch 'found
        (maphash (lambda (_path meta)
                   (when (equal (plist-get meta :title) title)
                     (let ((tags (plist-get meta :tags)))
                       (dolist (group grove-graph-tag-groups)
                         (when (member (car group) tags)
                           (setq color (cdr group))
                           (throw 'found t))))))
                 grove--cache)))
    color))

(defun grove-graph-fa2--init-sim (adjacency)
  "Initialise node arrays, edge tuples, and pre-compute the static mass matrix."
  (let ((node-list nil)
        (name-to-idx (make-hash-table :test #'equal))
        (idx 0))
    (dolist (entry adjacency)
      (let ((source (car entry)) 
            (targets (cdr entry)))
        (unless (gethash source name-to-idx)
          (puthash source idx name-to-idx)
          (push (vector source 
                        (truncate (* (grove-graph-fa2--hash-pos source "x") 256.0)) 
                        (truncate (* (grove-graph-fa2--hash-pos source "y") 256.0)) 
                        0 0 
                        (+ 1 (length targets)) 
                        (grove-graph-fa2--node-color source)) node-list)
          (cl-incf idx))
        (dolist (target targets)
          (unless (gethash target name-to-idx)
            (puthash target idx name-to-idx)
            (push (vector target 
                          (truncate (* (grove-graph-fa2--hash-pos target "x") 256.0)) 
                          (truncate (* (grove-graph-fa2--hash-pos target "y") 256.0)) 
                          0 0 
                          2 
                          (grove-graph-fa2--node-color target)) node-list)
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
      (setq grove-graph-fa2--bg-edges edge-list))

    (let* ((len (length grove-graph-fa2--bg-nodes))
           (matrix (make-vector (* len len) 0)))
      (dotimes (i len)
        (let ((ni (aref grove-graph-fa2--bg-nodes i)))
          (dotimes (j len)
            (when (> j i)
              (let ((nj (aref grove-graph-fa2--bg-nodes j)))
                (aset matrix (+ (* i len) j)
                      (truncate (* grove-graph-fa2--k-r (fa2-mass ni) (fa2-mass nj)))))))))
      (setq grove-graph-fa2--mass-matrix matrix))))

(defun grove-graph-fa2--wrap-text (text max-chars)
  (let ((words (split-string text " "))
        (lines nil)
        (current-line ""))
    (dolist (word words)
      (if (string= current-line "")
          (setq current-line word)
        (if (<= (+ (length current-line) 1 (length word)) max-chars)
            (setq current-line (concat current-line " " word))
          (push current-line lines)
          (setq current-line word))))
    (when (not (string= current-line ""))
      (push current-line lines))
    (nreverse lines)))

(defun grove-graph-fa2--physics-tick (max-frames)
  "Calculates physics using fixed-point maths, field extractions and AABB culling. Caching computed values where possible."
  (declare (speed 3))
  
  (let* ((t-load 0.0) (t-repulsion 0.0) (t-attraction 0.0)
         (t-integration 0.0) (t-write 0.0) (t-svg 0.0) (t-temp 0.0)
         
         (nodes grove-graph-fa2--bg-nodes)
         (edges grove-graph-fa2--bg-edges)
         (mass-matrix grove-graph-fa2--mass-matrix)
         (total-nodes (length nodes))
         (len (if (< grove-graph-fa2--bg-frame 100)
                  (max 1 (truncate (* total-nodes (/ (float (1+ grove-graph-fa2--bg-frame)) 100.0))))
                total-nodes))
         (a (max 2 (truncate (* 256.0 (- 1.0 (/ (float grove-graph-fa2--bg-frame) max-frames))))))
         
         (pos-x (make-vector total-nodes 0))
         (pos-y (make-vector total-nodes 0))
         (vel-x (make-vector total-nodes 0))
         (vel-y (make-vector total-nodes 0))
         
         (rep-x (make-vector total-nodes 0))
         (rep-y (make-vector total-nodes 0)))

    (dotimes (i total-nodes)
      (let ((n (aref nodes i)))
        (aset pos-x i (fa2-x n))
        (aset pos-y i (fa2-y n))
        (aset vel-x i (fa2-dx n))
        (aset vel-y i (fa2-dy n))))
    
    (let ((gc-cons-threshold most-positive-fixnum))
      (dotimes (i len)
        (let ((nix (aref pos-x i))
              (niy (aref pos-y i))
              (i-offset (* i total-nodes)))
          (cl-loop for j from (1+ i) below len do
                   (let* ((dx (- nix (aref pos-x j)))
                          (abs-dx (if (< dx 0) (- dx) dx)))
                     (when (< abs-dx 80954)
                       (let* ((dy (- niy (aref pos-y j)))
                              (abs-dy (if (< dy 0) (- dy) dy)))
                         ;; AABB FAST-FAIL 2
                         (when (< abs-dy 80954)
                           (let* ((max-d (if (> abs-dx abs-dy) abs-dx abs-dy))
                                  (min-d (if (> abs-dx abs-dy) abs-dy abs-dx))
                                  (dist (if (= max-d 0) 1 (+ max-d (ash min-d -1))))
                                  (dist-sq (+ (* dx dx) (* dy dy)))
                                  (dist-sq (if (< dist-sq 655360) 655360 dist-sq)))
                             (when (< dist-sq 6553600000)
                               (let* ((mass-mult (truncate (aref mass-matrix (+ i-offset j))))
                                      (num (ash (* a mass-mult) 16)) 
                                      (den (* dist dist-sq))
                                      (fdx (/ (* dx num) den))
                                      (fdy (/ (* dy num) den)))
                                 (aset rep-x i (+ (aref rep-x i) fdx))
                                 (aset rep-y i (+ (aref rep-y i) fdy))
                                 (aset rep-x j (- (aref rep-x j) fdx))
                                 (aset rep-y j (- (aref rep-y j) fdy)))))))))))))

    (let ((gc-cons-threshold most-positive-fixnum))
      (dotimes (_ grove-graph-fa2--substeps)
        
        (dotimes (i len)
          (aset vel-x i (+ (aref vel-x i) (aref rep-x i)))
          (aset vel-y i (+ (aref vel-y i) (aref rep-y i))))

        (dolist (edge edges)
          (when (and (< (car edge) len) (< (cdr edge) len))
            (let* ((u (car edge)) (v (cdr edge))
                   (dx (- (aref pos-x u) (aref pos-x v)))
                   (dy (- (aref pos-y u) (aref pos-y v)))
                   (abs-dx (if (< dx 0) (- dx) dx)) (abs-dy (if (< dy 0) (- dy) dy))
                   (max-d (if (> abs-dx abs-dy) abs-dx abs-dy))
                   (min-d (if (> abs-dx abs-dy) abs-dy abs-dx))
                   (dist (if (= max-d 0) 1 (+ max-d (ash min-d -1))))
                   (dist-diff (- dist 12800))
                   (num (* a dist-diff))
                   (den (ash dist 16))
                   (fdx (/ (* dx num) den))
                   (fdy (/ (* dy num) den)))
              (aset vel-x u (- (aref vel-x u) fdx)) (aset vel-y u (- (aref vel-y u) fdy))
              (aset vel-x v (+ (aref vel-x v) fdx)) (aset vel-y v (+ (aref vel-y v) fdy)))))

        (dotimes (i len)
          (let* ((nx (aref pos-x i)) (ny (aref pos-y i))
                 (abs-nx (if (< nx 0) (- nx) nx)) (abs-ny (if (< ny 0) (- ny) ny))
                 (max-n (if (> abs-nx abs-ny) abs-nx abs-ny))
                 (min-n (if (> abs-nx abs-ny) abs-ny abs-nx))
                 (dist (if (= max-n 0) 1 (+ max-n (ash min-n -1))))
                 (mass (truncate (fa2-mass (aref nodes i))))
                 (num (* a mass)) (den (ash dist 8))
                 (fdx (/ (* nx num) den)) (fdy (/ (* ny num) den)))
            (aset vel-x i (- (aref vel-x i) fdx)) (aset vel-y i (- (aref vel-y i) fdy))
            (let* ((vx (aref vel-x i)) (vy (aref vel-y i))
                   (abs-vx (if (< vx 0) (- vx) vx)) (abs-vy (if (< vy 0) (- vy) vy))
                   (max-v (if (> abs-vx abs-vy) abs-vx abs-vy))
                   (min-v (if (> abs-vx abs-vy) abs-vy abs-vx))
                   (speed (if (= max-v 0) 1 (+ max-v (ash min-v -1)))))
              (when (> speed 25)
                (let ((v-max 12800))
                  (aset vel-x i (/ (* vx v-max) (+ speed v-max)))
                  (aset vel-y i (/ (* vy v-max) (+ speed v-max))))))
            (aset pos-x i (+ nx (ash (aref vel-x i) -4)))
            (aset pos-y i (+ ny (ash (aref vel-y i) -4)))
            (let* ((horizon 61440) (horizon-start 49152)
                   (new-nx (aref pos-x i)) (new-ny (aref pos-y i))
                   (abs-new-nx (if (< new-nx 0) (- new-nx) new-nx))
                   (abs-new-ny (if (< new-ny 0) (- new-ny) new-ny))
                   (max-new (if (> abs-new-nx abs-new-ny) abs-new-nx abs-new-ny))
                   (min-new (if (> abs-new-nx abs-new-ny) abs-new-ny abs-new-nx))
                   (new-dist (if (= max-new 0) 1 (+ max-new (ash min-new -1)))))
              (when (> new-dist horizon)
                (let ((clamp-scale (/ (ash horizon 16) new-dist)))
                  (aset pos-x i (ash (* new-nx clamp-scale) -16))
                  (aset pos-y i (ash (* new-ny clamp-scale) -16))
                  (setq new-dist horizon)))
              (cond
               ((>= new-dist horizon) (aset vel-x i 0) (aset vel-y i 0))
               ((> new-dist horizon-start)
                (aset vel-x i (- (aref vel-x i) (ash (aref vel-x i) -2)))
                (aset vel-y i (- (aref vel-y i) (ash (aref vel-y i) -2))))
               (t
                (aset vel-x i (- (aref vel-x i) (ash (aref vel-x i) -6)))
                (aset vel-y i (- (aref vel-y i) (ash (aref vel-y i) -6))))))))))

    (dotimes (i total-nodes)
      (let ((n (aref nodes i)))
        (fa2-set-x n (aref pos-x i))
        (fa2-set-y n (aref pos-y i))
        (fa2-set-dx n (aref vel-x i))
        (fa2-set-dy n (aref vel-y i))))

    (let ((gc-cons-threshold most-positive-fixnum))
      (with-current-buffer grove-graph-fa2--bg-buffer
        (let* ((canvas-int (truncate grove-graph-fa2--canvas-size))
               (half-canvas (truncate (/ grove-graph-fa2--canvas-size 2.0)))
               (canvas (number-to-string canvas-int)))
          
          (insert "<svg width=\"" canvas "\" height=\"" canvas "\" viewBox=\"0 0 " canvas " " canvas "\" xmlns=\"http://www.w3.org/2000/svg\">\n")
          
          (dolist (edge edges)
            (when (and (< (car edge) len) (< (cdr edge) len))
              (let* ((u (aref nodes (car edge))) 
                     (v (aref nodes (cdr edge)))
                     (ux (number-to-string (+ (ash (fa2-x u) -8) half-canvas)))
                     (uy (number-to-string (+ (ash (fa2-y u) -8) half-canvas)))
                     (vx (number-to-string (+ (ash (fa2-x v) -8) half-canvas)))
                     (vy (number-to-string (+ (ash (fa2-y v) -8) half-canvas))))
                (insert "  <line x1=\"" ux "\" y1=\"" uy "\" x2=\"" vx "\" y2=\"" vy "\" stroke=\"#585b70\" stroke-width=\"2\" />\n"))))
          
          (dotimes (i len)
            (let* ((n (aref nodes i))
                   (nx-int (+ (ash (fa2-x n) -8) half-canvas))
                   (ny-int (+ (ash (fa2-y n) -8) half-canvas))
                   (nx (number-to-string nx-int))
                   (ny (number-to-string ny-int))
                   
                   (name-escaped (grove-graph-fa2--escape-xml (fa2-name n)))
                   (lines (grove-graph-fa2--wrap-text name-escaped 10))
                   (line-height 12)
                   (start-y (- ny-int 15 (* (1- (length lines)) (/ line-height 2)))))
              
              (insert "  <circle cx=\"" nx "\" cy=\"" ny "\" r=\"10\" fill=\"" (fa2-color n) "\" data-name=\"" name-escaped "\" />\n")
              
              (insert "  <text fill=\"#cdd6f4\" font-size=\"10\" text-anchor=\"middle\">\n")
              (let ((curr-y start-y))
                (dolist (line lines)
                  (insert "    <tspan x=\"" nx "\" y=\"" (number-to-string curr-y) "\">" line "</tspan>\n")
                  (cl-incf curr-y line-height)))
              (insert "  </text>\n")))
          
          (insert "</svg>\n<FRAME_SPLIT>\n"))))))

(defun grove-graph-fa2--hot-reload-player (buf bg-buffer)
  "Feeds newly rendered frames into the live player without restarting."
  (when (buffer-live-p buf)
    (let ((playback-buf (buffer-local-value 'grove-graph--playback-buffer buf)))
      (when (buffer-live-p playback-buf)
        (with-current-buffer playback-buf
          (let ((inhibit-read-only t)
                (offsets nil))
            (erase-buffer)
            (insert-buffer-substring bg-buffer) ;; Copy directly from memory (Zero disk I/O)
            (goto-char (point-min))
            (let ((start (point)))
              (while (search-forward "<FRAME_SPLIT>\n" nil t)
                (push (cons start (match-beginning 0)) offsets)
                (when (looking-at "\n") (forward-char 1))
                (setq start (point)))
              (when (< start (point-max)) 
                (push (cons start (point-max)) offsets)))
            ;; Dynamically grow the frontend's timeline vector!
            (with-current-buffer buf
              (setq-local grove-graph--frame-offsets (vconcat (nreverse offsets))))))))))

(defun grove-graph-fa2--render-chunk (cache-file hash-file target-hash target-buf max-frames playback-fps)
  "Cooperatively renders frames. Yields on user input or after budget.

   1. Get average after all nodes added to sim
   2. Estimate UI overhead 
   3. Start streaming once svg buffer depth established"
  (let ((chunk-end-time (time-add nil 0.05))
        (slice-start-time (float-time))
        (slice-start-frames grove-graph-fa2--frames-rendered)
        (frames-in-slice 0)
        (playback-ms (/ 1.0 playback-fps)))
    
    (let ((gc-cons-threshold most-positive-fixnum))
      (while (and (< grove-graph-fa2--frames-rendered max-frames)
                  (time-less-p nil chunk-end-time)
                  (not (input-pending-p)))
        (setq grove-graph-fa2--bg-frame grove-graph-fa2--frames-rendered)
        (grove-graph-fa2--physics-tick max-frames)
        (cl-incf grove-graph-fa2--frames-rendered)
        (cl-incf frames-in-slice)))
    
    (let* ((slice-duration (* (- (float-time) slice-start-time) 1000.0))
           ;; Calculate how many frames in this specific slice were beyond the 100 spawn-in
           (valid-frames (max 0 (- grove-graph-fa2--frames-rendered (max 100 slice-start-frames)))))
      
      ;; 1. Accumulate True Average (Only counts post-100 frames)
      (when (> valid-frames 0)
        (cl-incf grove-graph-fa2--heavy-frames valid-frames)
        (cl-incf grove-graph-fa2--heavy-time (* slice-duration (/ (float valid-frames) frames-in-slice))))
      
      (let ((cumulative-avg (if (> grove-graph-fa2--heavy-frames 0)
                                (/ grove-graph-fa2--heavy-time grove-graph-fa2--heavy-frames)
                              0.0)))
        
        
        (unless (or grove-graph-fa2--playback-started
                    (< grove-graph-fa2--frames-rendered 100)
                    (= grove-graph-fa2--heavy-frames 0))
          (let* ((tg (/ cumulative-avg 1000.0))
                 (predicted-tg (+ tg grove-graph-fa2-playback-penalty))
                 (safe-buffer
                  (if (<= predicted-tg playback-ms)
                      1 
                    (ceiling (* max-frames (/ (- predicted-tg playback-ms) predicted-tg))))))
            
            (when (>= grove-graph-fa2--frames-rendered (+ 100 safe-buffer))
              (setq grove-graph-fa2--playback-started t)
              (with-current-buffer grove-graph-fa2--bg-buffer
                (let ((coding-system-for-write 'utf-8)) 
                  (write-region (point-min) (point-max) cache-file nil 'silent)))
              (when (buffer-live-p target-buf) 
                (grove-graph-fa2--load-and-play target-buf cache-file)))))
        
        (when (and grove-graph-fa2--playback-started (< grove-graph-fa2--frames-rendered max-frames))
          (grove-graph-fa2--hot-reload-player target-buf grove-graph-fa2--bg-buffer))))
    
    (if (< grove-graph-fa2--frames-rendered max-frames)
        (setq grove-graph-fa2--bg-timer 
              (run-at-time 0 nil #'grove-graph-fa2--render-chunk cache-file hash-file target-hash target-buf max-frames playback-fps))
      
      (with-current-buffer grove-graph-fa2--bg-buffer
        (let ((coding-system-for-write 'utf-8)) 
          (write-region (point-min) (point-max) cache-file nil 'silent)))
      (with-temp-file hash-file (insert target-hash))
      (message "Background cache render complete.")
      
      (when (and (buffer-live-p target-buf) (not grove-graph-fa2--playback-started))
        (grove-graph-fa2--load-and-play target-buf cache-file))
      
      (kill-buffer grove-graph-fa2--bg-buffer))))

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
  "Initialise the cooperative physics background worker or load from cache."
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

        (message "Rendering cache cooperatively (streaming to UI)...")
        (when grove-graph-fa2--bg-timer (cancel-timer grove-graph-fa2--bg-timer))
        (when (buffer-live-p grove-graph-fa2--bg-buffer) (kill-buffer grove-graph-fa2--bg-buffer))
        
        (setq grove-graph-fa2--bg-buffer (generate-new-buffer " *grove-fa2-bg*"))
        (grove-graph-fa2--init-sim adjacency)
        
        ;; Initialise streaming thresholds
        (setq grove-graph-fa2--frames-rendered 0
              grove-graph-fa2--heavy-frames 0
              grove-graph-fa2--heavy-time 0.0
              grove-graph-fa2--playback-started nil
              grove-graph-fa2--start-time (current-time))
        
        ;; Kick off the time-slicer: 840 frames at 60 FPS
        (setq grove-graph-fa2--bg-timer
              (run-at-time 0 nil #'grove-graph-fa2--render-chunk data-file hash-file current-hash buf 840 60.0))))))

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
