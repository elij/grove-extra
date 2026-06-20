;;; grove-graph-fa2.el --- ForceAtlas2 pure-elisp background-cached engine -*- lexical-binding: t -*-

(eval-when-compile
  (when (boundp 'comp-speed)
    (setq comp-speed 3)))

(require 'cl-lib)
(require 'grove-core)
(require 'json)

(declare-function grove-graph--smil-start "grove-graph")
(declare-function grove-graph--update-display "grove-graph")

(defconst grove-graph-fa2--substeps 10
  "The number of physics substeps per frame.")

(defconst grove-graph-fa2--k-r 50.0
  "The repulsion constant.")

(defconst grove-graph-fa2--k-g 0.005
  "The gravity constant.")

(defconst grove-graph-fa2--k-a 0.005
  "The attraction constant.")

(defconst grove-graph-fa2--target-dist 50.0
  "The target distance for attraction.")

(defconst grove-graph-fa2--friction 0.98
  "The damping friction coefficient.")

(defconst grove-graph-fa2--time-step 0.05
  "The simulation time step.")

(defconst grove-graph-fa2--max-speed 50.0
  "The maximum speed limit for a node.")

(defconst grove-graph-fa2--canvas-size 500.0
  "The size of the square rendering canvas.")

(defconst grove-graph-fa2--event-horizon 240.0
  "The threshold distance beyond which forces fade.")

(defvar grove-graph--current-frame)
(defvar grove-graph--frame-offsets)
(defvar grove-graph--playback-buffer)
(defvar grove-graph--raw-svg)

(defvar graph-fa2-node-clicked-functions nil
  "Abnormal hook run when a graph node is clicked.
Each hook function receives the opaque node identifier string as its sole argument.")

(cl-defstruct graph-fa2-ctx
  "State structure for ForceAtlas2 physics simulation.
Contains node and edge definitions, pre-allocated vectors to minimise
garbage collection pressure, and running animation state."
  nodes
  edges
  mass-matrix
  pos-x
  pos-y
  vel-x
  vel-y
  rep-x
  rep-y
  bg-buffer
  bg-frame
  bg-timer
  frames-rendered
  heavy-frames
  heavy-time
  playback-started
  start-time)

(defvar-local grove-graph-fa2-ctx nil
  "Buffer-local ForceAtlas2 simulation context.")

(defsubst fa2-id (n)
  "Return the identifier of node N."
  (aref n 0))

(defsubst fa2-label (n)
  "Return the label of node N."
  (aref n 1))

(defsubst fa2-x (n)
  "Return the x coordinate of node N."
  (aref n 2))

(defsubst fa2-y (n)
  "Return the y coordinate of node N."
  (aref n 3))

(defsubst fa2-dx (n)
  "Return the x velocity of node N."
  (aref n 4))

(defsubst fa2-dy (n)
  "Return the y velocity of node N."
  (aref n 5))

(defsubst fa2-mass (n)
  "Return the mass of node N."
  (aref n 6))

(defsubst fa2-colour (n)
  "Return the colour string of node N."
  (aref n 7))

(defsubst fa2-radius (n)
  "Return the radius of node N."
  (aref n 8))

(defsubst fa2-set-x (n v)
  "Set the x coordinate of node N to V."
  (aset n 2 v))

(defsubst fa2-set-y (n v)
  "Set the y coordinate of node N to V."
  (aset n 3 v))

(defsubst fa2-set-dx (n v)
  "Set the x velocity of node N to V."
  (aset n 4 v))

(defsubst fa2-set-dy (n v)
  "Set the y velocity of node N to V."
  (aset n 5 v))

(defun grove-graph-fa2--escape-xml (str)
  "Escape XML characters in STR."
  (let ((s (replace-regexp-in-string "&" "&amp;" str t t)))
    (setq s (replace-regexp-in-string "<" "&lt;" s t t))
    (setq s (replace-regexp-in-string ">" "&gt;" s t t))
    (setq s (replace-regexp-in-string "\"" "&quot;" s t t))
    s))

(defun grove-graph-fa2--unescape-xml (str)
  "Restore standard characters from XML-escaped node names.
This is the inverse of the XML escape function."
  (let ((s (replace-regexp-in-string "&quot;" "\"" str t t)))
    (setq s (replace-regexp-in-string "&gt;" ">" s t t))
    (setq s (replace-regexp-in-string "&lt;" "<" s t t))
    (setq s (replace-regexp-in-string "&amp;" "&" s t t))
    s))

(defun grove-graph-fa2--hash-pos (str offset)
  "Return a pseudo-random number between -500 and 500 based on STR and OFFSET."
  (if (and (boundp 'grove-graph-deterministic-positions) grove-graph-deterministic-positions)
      (- (mod (string-to-number (substring (secure-hash 'md5 (concat str offset)) 0 8) 16) 1000) 500.0)
    (- (random 1000.0) 500.0)))

(defun grove-graph-fa2--create-ctx (nodes edges)
  "Create and initialise a graph-fa2-ctx struct from generic NODES and EDGES.
This pre-allocates the six physics vectors to completely eliminate
garbage collection pressure during background rendering."
  (let ((degree-map (make-hash-table :test #'equal)))
    (seq-doseq (edge edges)
      (let ((src (car edge))
            (tgt (cdr edge)))
        (puthash src (1+ (gethash src degree-map 0)) degree-map)
        (puthash tgt (1+ (gethash tgt degree-map 0)) degree-map)))
    (let* ((id-to-idx (make-hash-table :test #'equal))
           (len (length nodes))
           (internal-nodes (make-vector len nil))
           (idx 0))
      (seq-doseq (n nodes)
        (let* ((id (plist-get n :id))
               (label (plist-get n :label))
               (colour (or (plist-get n :colour) (plist-get n :color) "#89b4fa"))
               (radius (or (plist-get n :radius) 10.0))
               (mass (+ 1 (gethash id degree-map 0)))
               (x (truncate (* (grove-graph-fa2--hash-pos id "x") 256.0)))
               (y (truncate (* (grove-graph-fa2--hash-pos id "y") 256.0))))
          (puthash id idx id-to-idx)
          (aset internal-nodes idx (vector id label x y 0 0 mass colour radius))
          (cl-incf idx)))
      (let (internal-edges)
        (seq-doseq (edge edges)
          (let* ((src (car edge))
                 (tgt (cdr edge))
                 (s-idx (gethash src id-to-idx))
                 (t-idx (gethash tgt id-to-idx)))
            (when (and s-idx t-idx)
              (push (cons s-idx t-idx) internal-edges))))
        (let* ((matrix (make-vector (* len len) 0)))
          (dotimes (i len)
            (let ((ni (aref internal-nodes i)))
              (dotimes (j len)
                (when (> j i)
                  (let ((nj (aref internal-nodes j)))
                    (aset matrix (+ (* i len) j)
                          (truncate (* 50.0 (fa2-mass ni) (fa2-mass nj)))))))))
          (let* ((pos-x (make-vector len 0))
                 (pos-y (make-vector len 0))
                 (vel-x (make-vector len 0))
                 (vel-y (make-vector len 0))
                 (rep-x (make-vector len 0))
                 (rep-y (make-vector len 0)))
            (dotimes (i len)
              (let ((n (aref internal-nodes i)))
                (aset pos-x i (fa2-x n))
                (aset pos-y i (fa2-y n))))
            (make-graph-fa2-ctx
             :nodes internal-nodes
             :edges (nreverse internal-edges)
             :mass-matrix matrix
             :pos-x pos-x
             :pos-y pos-y
             :vel-x vel-x
             :vel-y vel-y
             :rep-x rep-x
             :rep-y rep-y
             :bg-frame 0
             :frames-rendered 0
             :heavy-frames 0
             :heavy-time 0.0
             :playback-started nil
             :start-time (current-time))))))))

(defun grove-graph-fa2--wrap-text (text max-chars)
  "Wrap TEXT to lines of at most MAX-CHARS."
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

(defun grove-graph-fa2--render-empty (ctx)
  "Render zero-node svg."
  (let ((gc-cons-threshold most-positive-fixnum))
    (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
      (let* ((canvas-int (truncate 500.0))
             (canvas (number-to-string canvas-int)))
        (insert "<svg width=\"" canvas "\" height=\"" canvas "\" viewBox=\"0 0 " canvas " " canvas "\" xmlns=\"http://www.w3.org/2000/svg\">\n</svg>\n<FRAME_SPLIT>\n")))))

(defun grove-graph-fa2--compute-repulsion (ctx len a)
  "Compute repulsion between all active node pairs."
  (let* ((pos-x (graph-fa2-ctx-pos-x ctx))
         (pos-y (graph-fa2-ctx-pos-y ctx))
         (rep-x (graph-fa2-ctx-rep-x ctx))
         (rep-y (graph-fa2-ctx-rep-y ctx))
         (mass-matrix (graph-fa2-ctx-mass-matrix ctx))
         (total-nodes (length (graph-fa2-ctx-nodes ctx))))
    (fillarray rep-x 0.0)
    (fillarray rep-y 0.0)
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
                         (when (< abs-dy 80954)
                           (let* ((max-d (if (> abs-dx abs-dy) abs-dx abs-dy))
                                  (min-d (if (> abs-dx abs-dy) abs-dy abs-dx))
                                  (dist (if (= max-d 0) 1 (+ max-d (ash (truncate min-d) -1))))
                                  (dist-sq (+ (* dx dx) (* dy dy)))
                                  (dist-sq (if (< dist-sq 655360) 655360 dist-sq)))
                             (when (< dist-sq 6553600000)
                               (let* ((mass-mult (truncate (aref mass-matrix (+ i-offset j))))
                                      (num (ash (truncate (* a mass-mult)) 16))
                                      (den (* dist dist-sq))
                                      (fdx (/ (* dx num) den))
                                      (fdy (/ (* dy num) den)))
                                 (aset rep-x i (+ (aref rep-x i) fdx))
                                 (aset rep-y i (+ (aref rep-y i) fdy))
                                 (aset rep-x j (- (aref rep-x j) fdx))
                                 (aset rep-y j (- (aref rep-y j) fdy)))))))))))))))

(defun grove-graph-fa2--apply-repulsion (ctx len)
  "Add the accumulated repulsion."
  (let ((vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (rep-x (graph-fa2-ctx-rep-x ctx))
        (rep-y (graph-fa2-ctx-rep-y ctx)))
    (dotimes (i len)
      (aset vel-x i (+ (aref vel-x i) (aref rep-x i)))
      (aset vel-y i (+ (aref vel-y i) (aref rep-y i))))))

(defun grove-graph-fa2--apply-attraction (ctx len a)
  "Calculate  edge-based attraction."
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (edges (graph-fa2-ctx-edges ctx)))
    (dolist (edge edges)
      (when (and (< (car edge) len) (< (cdr edge) len))
        (let* ((u (car edge))
               (v (cdr edge))
               (dx (- (aref pos-x u) (aref pos-x v)))
               (dy (- (aref pos-y u) (aref pos-y v)))
               (abs-dx (if (< dx 0) (- dx) dx))
               (abs-dy (if (< dy 0) (- dy) dy))
               (max-d (if (> abs-dx abs-dy) abs-dx abs-dy))
               (min-d (if (> abs-dx abs-dy) abs-dy abs-dx))
               (dist (if (= max-d 0) 1 (+ max-d (ash (truncate min-d) -1))))
               (dist-diff (- dist 12800))
               (num (* a dist-diff))
               (den (ash (truncate dist) 16))
               (fdx (/ (* dx num) den))
               (fdy (/ (* dy num) den)))
          (aset vel-x u (- (aref vel-x u) fdx))
          (aset vel-y u (- (aref vel-y u) fdy))
          (aset vel-x v (+ (aref vel-x v) fdx))
          (aset vel-y v (+ (aref vel-y v) fdy)))))))

(defun grove-graph-fa2--integrate-and-cull (ctx len a)
  "Process gravity, enforce speed limits, integrate positions, and cull nodes."
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (nodes (graph-fa2-ctx-nodes ctx)))
    (dotimes (i len)
      (let* ((nx (aref pos-x i))
             (ny (aref pos-y i))
             (abs-nx (if (< nx 0) (- nx) nx))
             (abs-ny (if (< ny 0) (- ny) ny))
             (max-n (if (> abs-nx abs-ny) abs-nx abs-ny))
             (min-n (if (> abs-nx abs-ny) abs-ny abs-nx))
             (dist (if (= max-n 0) 1 (+ max-n (ash (truncate min-n) -1))))
             (mass (truncate (fa2-mass (aref nodes i))))
             (num (* a mass))
             (den (ash (truncate dist) 8))
             (fdx (/ (* nx num) den))
             (fdy (/ (* ny num) den)))
        (aset vel-x i (- (aref vel-x i) fdx))
        (aset vel-y i (- (aref vel-y i) fdy))
        (let* ((vx (aref vel-x i))
               (vy (aref vel-y i))
               (abs-vx (if (< vx 0) (- vx) vx))
               (abs-vy (if (< vy 0) (- vy) vy))
               (max-v (if (> abs-vx abs-vy) abs-vx abs-vy))
               (min-v (if (> abs-vx abs-vy) abs-vy abs-vx))
               (speed (if (= max-v 0) 1 (+ max-v (ash (truncate min-v) -1)))))
          (when (> speed 25)
            (let ((v-max 12800))
              (aset vel-x i (/ (* (truncate vx) v-max) (+ speed v-max)))
              (aset vel-y i (/ (* (truncate vy) v-max) (+ speed v-max))))))
        (aset pos-x i (+ nx (ash (truncate (aref vel-x i)) -4)))
        (aset pos-y i (+ ny (ash (truncate (aref vel-y i)) -4)))
        (let* ((horizon 61440)
               (horizon-start 49152)
               (new-nx (aref pos-x i))
               (new-ny (aref pos-y i))
               (abs-new-nx (if (< new-nx 0) (- new-nx) new-nx))
               (abs-new-ny (if (< new-ny 0) (- new-ny) new-ny))
               (max-new (if (> abs-new-nx abs-new-ny) abs-new-nx abs-new-ny))
               (min-new (if (> abs-new-nx abs-new-ny) abs-new-ny abs-new-nx))
               (new-dist (if (= max-new 0) 1 (+ max-new (ash (truncate min-new) -1)))))
          (when (> new-dist horizon)
            (let ((clamp-scale (/ (ash horizon 16) new-dist)))
              (aset pos-x i (ash (truncate (* new-nx clamp-scale)) -16))
              (aset pos-y i (ash (truncate (* new-ny clamp-scale)) -16))
              (setq new-dist horizon)))
          (cond
           ((>= new-dist horizon)
            (aset vel-x i 0)
            (aset vel-y i 0))
           ((> new-dist horizon-start)
            (aset vel-x i (- (aref vel-x i) (ash (truncate (aref vel-x i)) -2)))
            (aset vel-y i (- (aref vel-y i) (ash (truncate (aref vel-y i)) -2))))
           (t
            (aset vel-x i (- (aref vel-x i) (ash (truncate (aref vel-x i)) -6)))
            (aset vel-y i (- (aref vel-y i) (ash (truncate (aref vel-y i)) -6))))))))))

(defun grove-graph-fa2--sync-nodes (ctx total-nodes)
  "Sync  arrays with node structs."
  (let ((nodes (graph-fa2-ctx-nodes ctx))
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx)))
    (dotimes (i total-nodes)
      (let ((n (aref nodes i)))
        (fa2-set-x n (aref pos-x i))
        (fa2-set-y n (aref pos-y i))
        (fa2-set-dx n (aref vel-x i))
        (fa2-set-dy n (aref vel-y i))))))

(defun grove-graph-fa2--render-svg (ctx len)
  "Render the current layout arrays to an SVG string."
  (let* ((nodes (graph-fa2-ctx-nodes ctx))
         (edges (graph-fa2-ctx-edges ctx))
         (pos-x (graph-fa2-ctx-pos-x ctx))
         (pos-y (graph-fa2-ctx-pos-y ctx))
         (gc-cons-threshold most-positive-fixnum))
    (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
      (let* ((canvas-int (truncate 500.0))
             (half-canvas (truncate (/ 500.0 2.0)))
             (canvas (number-to-string canvas-int)))
        (insert "<svg width=\"" canvas "\" height=\"" canvas "\" viewBox=\"0 0 " canvas " " canvas "\" xmlns=\"http://www.w3.org/2000/svg\">\n")
        (dolist (edge edges)
          (when (and (< (car edge) len) (< (cdr edge) len))
            (let* ((u-idx (car edge))
                   (v-idx (cdr edge))
                   (ux (number-to-string (+ (ash (truncate (aref pos-x u-idx)) -8) half-canvas)))
                   (uy (number-to-string (+ (ash (truncate (aref pos-y u-idx)) -8) half-canvas)))
                   (vx (number-to-string (+ (ash (truncate (aref pos-x v-idx)) -8) half-canvas)))
                   (vy (number-to-string (+ (ash (truncate (aref pos-y v-idx)) -8) half-canvas))))
              (insert "  <line x1=\"" ux "\" y1=\"" uy "\" x2=\"" vx "\" y2=\"" vy "\" stroke=\"#585b70\" stroke-width=\"2\" />\n"))))
        (dotimes (i len)
          (let* ((n (aref nodes i))
                 (nx-int (+ (ash (truncate (aref pos-x i)) -8) half-canvas))
                 (ny-int (+ (ash (truncate (aref pos-y i)) -8) half-canvas))
                 (nx (number-to-string nx-int))
                 (ny (number-to-string ny-int))
                 (id (fa2-id n))
                 (label (fa2-label n))
                 (radius (fa2-radius n))
                 (colour (fa2-colour n))
                 (name-escaped (grove-graph-fa2--escape-xml label))
                 (lines (grove-graph-fa2--wrap-text name-escaped 10))
                 (line-height 12)
                 (start-y (- ny-int 15 (* (1- (length lines)) (/ line-height 2)))))
            (insert "  <circle cx=\"" nx "\" cy=\"" ny "\" r=\"" (number-to-string radius) "\" fill=\"" colour "\" data-name=\"" (grove-graph-fa2--escape-xml id) "\" />\n")
            (insert "  <text fill=\"#cdd6f4\" font-size=\"10\" text-anchor=\"middle\">\n")
            (let ((curr-y start-y))
              (dolist (line lines)
                (insert "    <tspan x=\"" nx "\" y=\"" (number-to-string curr-y) "\">" line "</tspan>\n")
                (cl-incf curr-y line-height)))
            (insert "  </text>\n")))
        (insert "</svg>\n<FRAME_SPLIT>\n")))))

(defun grove-graph-fa2--physics-tick (ctx max-frames)
  "Calculate ForceAtlas2 physics tick using pre-allocated arrays in CTX.

Evaluate node count and render empty context if devoid of data.
Determine active rendering slice and scale variables based on animation frames.
Delegate to the compute repulsion function to populate spacing arrays.
Run core physics iterations across attraction, integration, and bounds constraints.
Synchronise buffers to state and trigger background rendering."
  (let* ((nodes (graph-fa2-ctx-nodes ctx))
         (total-nodes (length nodes)))
    (if (= total-nodes 0)
        (grove-graph-fa2--render-empty ctx)
      (let* ((bg-frame (graph-fa2-ctx-bg-frame ctx))
             (len (if (< bg-frame 100)
                      (max 1 (truncate (* total-nodes (/ (float (1+ bg-frame)) 100.0))))
                    total-nodes))
             (a (max 2 (truncate (* 256.0 (- 1.0 (/ (float bg-frame) max-frames)))))))
        (grove-graph-fa2--compute-repulsion ctx len a)
        (let ((gc-cons-threshold most-positive-fixnum))
          (dotimes (_ 10)
            (grove-graph-fa2--apply-repulsion ctx len)
            (grove-graph-fa2--apply-attraction ctx len a)
            (grove-graph-fa2--integrate-and-cull ctx len a)))
        (grove-graph-fa2--sync-nodes ctx total-nodes)
        (grove-graph-fa2--render-svg ctx len)))))

(defun grove-graph-fa2--hot-reload-player (buf bg-buffer)
  "Feeds newly rendered frames into the live player without restarting."
  (when (buffer-live-p buf)
    (let ((playback-buf (buffer-local-value 'grove-graph--playback-buffer buf)))
      (when (buffer-live-p playback-buf)
        (with-current-buffer playback-buf
          (let ((inhibit-read-only t)
                (offsets nil))
            (erase-buffer)
            (insert-buffer-substring bg-buffer)
            (goto-char (point-min))
            (let ((start (point)))
              (while (search-forward "<FRAME_SPLIT>\n" nil t)
                (push (cons start (match-beginning 0)) offsets)
                (when (looking-at "\n") (forward-char 1))
                (setq start (point)))
              (when (< start (point-max))
                (push (cons start (point-max)) offsets)))
            (with-current-buffer buf
              (setq-local grove-graph--frame-offsets (vconcat (nreverse offsets))))))))))

(defun grove-graph-fa2--render-chunk (ctx cache-file hash-file target-hash target-buf max-frames playback-fps)
  "Cooperatively renders frames of the simulation in CTX and schedules the next chunk."
  (let ((chunk-end-time (time-add nil 0.05))
        (slice-start-time (float-time))
        (slice-start-frames (graph-fa2-ctx-frames-rendered ctx))
        (frames-in-slice 0)
        (playback-ms (/ 1.0 playback-fps)))
    (let ((gc-cons-threshold most-positive-fixnum))
      (while (and (< (graph-fa2-ctx-frames-rendered ctx) max-frames)
                  (time-less-p nil chunk-end-time)
                  (not (input-pending-p)))
        (setf (graph-fa2-ctx-bg-frame ctx) (graph-fa2-ctx-frames-rendered ctx))
        (grove-graph-fa2--physics-tick ctx max-frames)
        (setf (graph-fa2-ctx-frames-rendered ctx) (1+ (graph-fa2-ctx-frames-rendered ctx)))
        (cl-incf frames-in-slice)))
    (let* ((slice-duration (* (- (float-time) slice-start-time) 1000.0))
           (valid-frames (max 0 (- (graph-fa2-ctx-frames-rendered ctx) (max 100 slice-start-frames)))))
      (when (> valid-frames 0)
        (setf (graph-fa2-ctx-heavy-frames ctx) (+ (graph-fa2-ctx-heavy-frames ctx) valid-frames))
        (setf (graph-fa2-ctx-heavy-time ctx) (+ (graph-fa2-ctx-heavy-time ctx) (* slice-duration (/ (float valid-frames) frames-in-slice)))))
      (let ((cumulative-avg (if (> (graph-fa2-ctx-heavy-frames ctx) 0)
                                (/ (graph-fa2-ctx-heavy-time ctx) (graph-fa2-ctx-heavy-frames ctx))
                              0.0)))
        (unless (or (graph-fa2-ctx-playback-started ctx)
                    (< (graph-fa2-ctx-frames-rendered ctx) 100)
                    (= (graph-fa2-ctx-heavy-frames ctx) 0))
          (let* ((tg (/ cumulative-avg 1000.0))
                 (predicted-tg (+ tg 0.020))
                 (safe-buffer
                  (if (<= predicted-tg playback-ms)
                      1
                    (ceiling (* max-frames (/ (- predicted-tg playback-ms) predicted-tg))))))
            (when (>= (graph-fa2-ctx-frames-rendered ctx) (+ 100 safe-buffer))
              (setf (graph-fa2-ctx-playback-started ctx) t)
              (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
                (let ((coding-system-for-write 'utf-8))
                  (write-region (point-min) (point-max) cache-file nil 'silent)))
              (when (buffer-live-p target-buf)
                (grove-graph-fa2--load-and-play target-buf cache-file)))))
        (when (and (graph-fa2-ctx-playback-started ctx) (< (graph-fa2-ctx-frames-rendered ctx) max-frames))
          (grove-graph-fa2--hot-reload-player target-buf (graph-fa2-ctx-bg-buffer ctx)))))
    (if (< (graph-fa2-ctx-frames-rendered ctx) max-frames)
        (setf (graph-fa2-ctx-bg-timer ctx)
              (run-at-time 0 nil #'grove-graph-fa2--render-chunk ctx cache-file hash-file target-hash target-buf max-frames playback-fps))
      (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
        (let ((coding-system-for-write 'utf-8))
          (write-region (point-min) (point-max) cache-file nil 'silent)))
      (with-temp-file hash-file (insert target-hash))
      (message "Background cache render complete.")
      (when (and (buffer-live-p target-buf) (not (graph-fa2-ctx-playback-started ctx)))
        (grove-graph-fa2--load-and-play target-buf cache-file))
      (kill-buffer (graph-fa2-ctx-bg-buffer ctx)))))

(defun graph-fa2-click-node (event)
  "Handle a mouse click on the SVG and run the abnormal hooks with the node identifier.
Extracts the data-name text property from the SVG at the exact coordinate of the click
and passes the resulting string to the hook graph-fa2-node-clicked-functions."
  (interactive "e")
  (let* ((posn (event-start event))
         (image-coords (posn-object-x-y posn))
         (image-size (posn-object-width-height posn)))
    (when (and image-coords image-size)
      (with-current-buffer (window-buffer (posn-window posn))
        (let ((raw-svg grove-graph--raw-svg))
          (when raw-svg
            (let ((nodes nil)
                  (start 0))
              (while (string-match "<circle cx=\"\\([0-9.-]+\\)\" cy=\"\\([0-9.-]+\\)\"[^>]*data-name=\"\\([^\"]+\\)\"" raw-svg start)
                (push (vector (string-to-number (match-string 1 raw-svg))
                              (string-to-number (match-string 2 raw-svg))
                              (match-string 3 raw-svg))
                      nodes)
                (setq start (match-end 0)))
              (let* ((img-w (max 1.0 (float (car image-size))))
                     (img-h (max 1.0 (float (cdr image-size))))
                     (min-dim (min img-w img-h))
                     (pad-x (/ (- img-w min-dim) 2.0))
                     (pad-y (/ (- img-h min-dim) 2.0))
                     (active-x (- (car image-coords) pad-x))
                     (active-y (- (cdr image-coords) pad-y)))
                (when (and (>= active-x 0) (<= active-x min-dim)
                           (>= active-y 0) (<= active-y min-dim))
                  (let* ((scale (/ 500.0 min-dim))
                         (mouse-x (* active-x scale))
                         (mouse-y (* active-y scale))
                         (nodes-vec (vconcat nodes))
                         (len (length nodes-vec))
                         (closest-node nil)
                         (min-dist-sq 900.0))
                    (dotimes (i len)
                      (let* ((n (aref nodes-vec i))
                             (nx (aref n 0))
                             (ny (aref n 1))
                             (dx (- mouse-x nx))
                             (dy (- mouse-y ny))
                             (dist-sq (+ (* dx dx) (* dy dy))))
                        (when (< dist-sq min-dist-sq)
                          (setq min-dist-sq dist-sq)
                          (setq closest-node (aref n 2)))))
                    (if closest-node
                        (progn
                          (run-hook-with-args 'graph-fa2-node-clicked-functions (grove-graph-fa2--unescape-xml closest-node))))))))))))))

(defvar graph-fa2-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'graph-fa2-click-node)
    (define-key map (kbd "<down-mouse-1>") #'graph-fa2-click-node)
    map)
  "Local keymap for the ForceAtlas2 animated graph buffer.")

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
        (use-local-map (make-composed-keymap graph-fa2-keymap (current-local-map)))
        (grove-graph--update-display)
        (message "Graph playback started.")
        (grove-graph--smil-start)))))

(defun grove-graph-fa2--plist-to-alist (item)
  "Convert a property list ITEM to an association list if it is a property list.
This guarantees deterministic JSON encoding across different Emacs versions."
  (if (and (listp item) (keywordp (car item)))
      (let (alist)
        (while item
          (let* ((key (car item))
                 (val (cadr item))
                 (key-str (replace-regexp-in-string "^:" "" (symbol-name key))))
            (push (cons key-str val) alist))
          (setq item (cddr item)))
        (nreverse alist))
    item))

;;;###autoload
(cl-defun grove-graph-fa2-start (buf nodes edges &key cache-dir)
  "Initialise the cooperative physics background worker or load from cache.
This creates the context struct, sets up the background buffer, pre-allocates vectors,
and starts the rendering thread."
  (let* ((resolved-cache-dir (or cache-dir (expand-file-name ".cache" (and (boundp 'grove-directory) grove-directory))))
         (hash-file (expand-file-name "fa2-graph.hash" resolved-cache-dir))
         (data-file (expand-file-name "fa2-graph.dat" resolved-cache-dir))
         (normalised-nodes (mapcar #'grove-graph-fa2--plist-to-alist nodes))
         (normalised-edges (mapcar (lambda (e) (list (car e) (cdr e))) edges))
         (payload (list normalised-nodes normalised-edges))
         (json-payload (json-encode payload))
         (current-hash (secure-hash 'md5 json-payload))
         (cached-hash (when (file-exists-p hash-file)
                        (with-temp-buffer
                          (insert-file-contents hash-file)
                          (string-trim (buffer-string))))))
    (unless (file-exists-p resolved-cache-dir)
      (make-directory resolved-cache-dir t))
    (if (and cached-hash (string= current-hash cached-hash) (file-exists-p data-file))
        (progn
          (message "Loading cached graph...")
          (grove-graph-fa2--load-and-play buf data-file))
      (message "Rendering cache cooperatively (streaming to UI)...")
      (let ((old-ctx (with-current-buffer buf (and (boundp 'grove-graph-fa2-ctx) grove-graph-fa2-ctx))))
        (when old-ctx
          (when (graph-fa2-ctx-bg-timer old-ctx)
            (cancel-timer (graph-fa2-ctx-bg-timer old-ctx)))
          (when (buffer-live-p (graph-fa2-ctx-bg-buffer old-ctx))
            (kill-buffer (graph-fa2-ctx-bg-buffer old-ctx)))))
      (let ((ctx (grove-graph-fa2--create-ctx nodes edges)))
        (with-current-buffer buf
          (setq-local grove-graph-fa2-ctx ctx))
        (setf (graph-fa2-ctx-bg-buffer ctx) (generate-new-buffer " *grove-fa2-bg*"))
        (setf (graph-fa2-ctx-bg-timer ctx)
              (run-at-time 0 nil #'grove-graph-fa2--render-chunk ctx data-file hash-file current-hash buf 840 60.0))))))

;;;###autoload
(defun grove-graph-fa2-clear-cache (&optional cache-dir)
  "Clears the background render cache to force a fresh physics simulation."
  (interactive)
  (let* ((resolved-cache-dir (or cache-dir (expand-file-name ".cache" grove-directory)))
         (hash-file (expand-file-name "fa2-graph.hash" resolved-cache-dir))
         (data-file (expand-file-name "fa2-graph.dat" resolved-cache-dir)))
    (when (file-exists-p hash-file) (delete-file hash-file))
    (when (file-exists-p data-file) (delete-file data-file))
    (message "ForceAtlas2 cache cleared. Run grove-graph to regenerate.")))

(provide 'grove-graph-fa2)
;;; grove-graph-fa2.el ends here
