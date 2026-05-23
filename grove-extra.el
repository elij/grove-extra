;;; grove-extra.el --- Unofficial extensions for Grove -*- lexical-binding: t -*-

;; Author: Elijah Charles
;; Version: 0.2.1
;; Package-Requires: ((emacs "29.1") (grove "0.1.0"))
;; Description: Adds Markdown support, ForceAtlas2, Mermaid, and SVG scaling to Grove.

(require 'grove)
(require 'grove-core)
(require 'grove-graph)
(require 'grove-link)
(require 'grove-search)
(require 'grove-tree)
(require 'grove-daily)
(require 'grove-capture)
(require 'grove-backlink)
(require 'json)

;; Try to load FA2 engine if available
(require 'grove-graph-fa2 nil t)

;;;; ===========================================================================
;;;; CUSTOMISATIONS & VARIABLES
;;;; ===========================================================================

(defgroup grove-extra nil
  "Extra customisations for Grove."
  :group 'grove)

(defcustom grove-file-extensions '("org" "md")
  "List of allowed file extensions for grove notes."
  :type '(repeat string)
  :group 'grove-extra)

(defcustom grove-default-extension "org"
  "Default file extension for new notes."
  :type 'string
  :group 'grove-extra)

(defcustom grove-graph-renderer 'dot
  "The underlying engine used to render the graph.
Valid options: `dot' (Graphviz), `mmdr' (Mermaid), `fa2' (Animated Physics)."
  :type '(choice (const dot) (const mmdr) (const fa2))
  :group 'grove-extra)

(defcustom grove-graph-mmdr-executable "mmdr"
  "Path to the mmdr executable."
  :type 'string
  :group 'grove-extra)

(defcustom grove-graph-mmdr-direction "TD"
  "Graph direction when using the mmdr renderer (TD, LR, RL, BT, etc)."
  :type 'string
  :group 'grove-extra)

(defcustom grove-graph-default-zoom 4.5
  "The default zoom scale for the graph upon opening."
  :type 'float
  :group 'grove-extra)

;; Graph State Variables
(defvar-local grove-graph--scale 1.0)
(defvar-local grove-graph--raw-svg nil)
(defvar-local grove-graph--current-frame 0)
(defvar-local grove-graph--smil-timer nil)
(defvar-local grove-graph--frame-vector nil)
(defvar-local grove-graph--playback-buffer nil)
(defvar-local grove-graph--frame-offsets nil)

;; Override the Link Regex to support Aliases globally
(setq grove-link--regexp "\\[\\[\\([^]\n|]+\\)\\(?:|[^]\n]+\\|\\]\\[[^]\n]+\\)?\\]\\]")

;;;; ===========================================================================
;;;; CORE UTILITIES & PREDICATES
;;;; ===========================================================================

(defun grove-extra--lock-sidebar-windows (&rest _)
  "Make Grove sidebar windows strongly dedicated to prevent buffer swapping."
  (dolist (buf-name '("*grove-tree*" "*grove-graph*"))
    (let ((win (get-buffer-window buf-name)))
      (when win
        (set-window-dedicated-p win t)))))

(defun grove-extra--make-ci-regexp (str)
  "Create a case-insensitive regexp string from STR."
  (mapconcat (lambda (c)
               (let ((s (char-to-string c)))
                 (if (string-match "[A-Za-z]" s)
                     (format "[%s%s]" (upcase s) (downcase s))
                   (regexp-quote s))))
             (format "%s" str) ""))

(defun grove-extra--file-extension-regexp ()
  "Return a regular expression matching allowed file extensions (Case Insensitive)."
  (let ((valid-exts (if (listp grove-file-extensions)
                        grove-file-extensions
                      (list grove-file-extensions))))
    (concat "\\`[^.].*\\.\\(" 
            (mapconcat #'grove-extra--make-ci-regexp 
                       (mapcar (lambda (s) (format "%s" s)) valid-exts) 
                       "\\|") 
            "\\)\\'")))

(defun grove-extra--valid-extension-p (filename)
  "Return non-nil if FILENAME has an allowed extension (Case Insensitive)."
  (let ((ext (file-name-extension filename))
        (valid-exts (if (listp grove-file-extensions)
                        grove-file-extensions
                      (list grove-file-extensions))))
    (and ext (member (downcase ext) (mapcar (lambda (s) (downcase (format "%s" s))) valid-exts)))))

(defun grove-extra--turn-on-hook ()
  "Enable grove-mode if the file belongs to the vault and our mode is active."
  (when (and grove-extra-mode
             (buffer-file-name)
             (grove-file-p (buffer-file-name)))
    (grove-mode 1)))

;;;; ===========================================================================
;;;; BUFFER-LOCAL MINOR MODES (UI & STATE)
;;;; ===========================================================================

;;; --- Graph Extension Mode ---

(defvar grove-extra-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'grove-graph-zoom-in)
    (define-key map (kbd "-") #'grove-graph-zoom-out)
    (define-key map (kbd "0") #'grove-graph-zoom-reset)
    (define-key map (kbd "<wheel-up>") #'grove-graph-zoom-in)
    (define-key map (kbd "<wheel-down>") #'grove-graph-zoom-out)
    map)
  "Keymap overriding `grove-graph-mode-map` with zooming tools.")

(defun grove-extra--graph-cleanup ()
  "Clean up playback buffers and timers when the graph is closed."
  (when (buffer-live-p grove-graph--playback-buffer)
    (kill-buffer grove-graph--playback-buffer))
  (when (fboundp 'grove-graph--smil-stop)
    (grove-graph--smil-stop)))

(define-minor-mode grove-extra-graph-mode
  "Buffer-local minor mode for Grove Graph UI enhancements."
  :init-value nil
  :lighter " Graph+"
  :keymap grove-extra-graph-mode-map
  (if grove-extra-graph-mode
      (progn
        (setq-local cursor-type nil)
        (setq-local bidi-display-reordering nil)
        (setq-local grove-extra--face-cookie 
                    (face-remap-add-relative 'default :background grove-graph-bg-color))
        (add-hook 'kill-buffer-hook #'grove-extra--graph-cleanup nil t)
        (add-hook 'window-size-change-functions #'grove-graph--update-display nil t))
    (progn
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'bidi-display-reordering)
      (when (boundp 'grove-extra--face-cookie)
        (face-remap-remove-relative grove-extra--face-cookie))
      (remove-hook 'kill-buffer-hook #'grove-extra--graph-cleanup t)
      (remove-hook 'window-size-change-functions #'grove-graph--update-display t))))

(defun grove-extra--enable-graph-mode ()
  "Turn on the extra graph features if the global mode is active."
  (when grove-extra-mode (grove-extra-graph-mode 1)))

;;; --- Capture Extension Mode ---

(define-minor-mode grove-extra-capture-mode
  "Buffer-local minor mode for Grove Capture enhancements."
  :init-value nil
  :lighter " Cap+"
  (if grove-extra-capture-mode
      (progn
        (if (string= (downcase grove-default-extension) "md")
            (when (fboundp 'markdown-mode) (markdown-mode))
          (when (fboundp 'org-mode) (org-mode)))
        (setq-local header-line-format
                    (substitute-command-keys
                     "Capture: \\[grove-capture-finalize] to save, \\[grove-capture-cancel] to discard")))
    (progn
      (kill-local-variable 'header-line-format))))

(defun grove-extra--enable-capture-mode ()
  "Turn on the extra capture features if the global mode is active."
  (when grove-extra-mode (grove-extra-capture-mode 1)))

;;;; ===========================================================================
;;;; AROUND ADVICE WRAPPERS (CORE LOGIC)
;;;; ===========================================================================

(defun grove-extra-around-file-p (orig-fun file)
  (if grove-extra-mode
      (and grove-directory file
           (grove-extra--valid-extension-p file)
           (string-prefix-p (expand-file-name grove-directory) (expand-file-name file)))
    (funcall orig-fun file)))

(defun grove-extra-around-parse-note (orig-fun file)
  (if grove-extra-mode
      (let ((mtime (file-attribute-modification-time (file-attributes file)))
            title tags links)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward "^\\(?:#\\+title:\\|title:\\|#\\)\\s-*\\(.+\\)" nil t)
            (setq title (string-trim (match-string 1) "[ \t\n\r\"]+")))
          (goto-char (point-min))
          (when (re-search-forward "^\\(?:#\\+filetags:\\|tags:\\)\\s-*\\(.+\\)" nil t)
            (let ((raw-tags (match-string 1)))
              (setq raw-tags (replace-regexp-in-string "[\\[\\]\"']" "" raw-tags))
              (setq tags (split-string raw-tags "[:,]\\s-*" t "\\s-*"))))
          (setq tags (grove--merge-tags tags (grove--collect-inline-tags)))
          (goto-char (point-min))
          (while (re-search-forward grove-link--regexp nil t)
            (let ((target (match-string-no-properties 1)))
              (push target links)))
          (list :title title :tags tags :links (nreverse links) :mtime mtime)))
    (funcall orig-fun file)))

(defun grove-extra-around-refresh-cache (orig-fun)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (let ((files (directory-files-recursively grove-directory (grove-extra--file-extension-regexp)))
              (seen (make-hash-table :test #'equal)))
          (dolist (file files)
            (puthash file t seen)
            (let* ((mtime (file-attribute-modification-time (file-attributes file)))
                   (cached (gethash file grove--cache)))
              (when (or (null cached) (time-less-p (plist-get cached :mtime) mtime))
                (puthash file (grove--parse-note file) grove--cache))))
          (maphash (lambda (file _meta) (unless (gethash file seen) (remhash file grove--cache))) grove--cache)))
    (funcall orig-fun)))

(defun grove-extra-around-tree-list-entries (orig-fun directory depth)
  (if grove-extra-mode
      (let (dirs files)
        (dolist (file (directory-files directory t))
          (let ((name (file-name-nondirectory file)))
            (unless (string-prefix-p "." name)
              (if (file-directory-p file)
                  (push (make-grove-tree-node :path file :name name :depth depth :directory-p t :expanded-p nil) dirs)
                (when (grove-extra--valid-extension-p name)
                  (push (make-grove-tree-node :path file :name (file-name-sans-extension name) :depth depth :directory-p nil :expanded-p nil) files))))))
        (append (sort dirs (lambda (a b) (string< (grove-tree-node-name a) (grove-tree-node-name b))))
                (sort files (lambda (a b) (string< (grove-tree-node-name a) (grove-tree-node-name b))))))
    (funcall orig-fun directory depth)))

(defun grove-extra-around-tree-item-count (orig-fun directory)
  (if grove-extra-mode
      (let ((count 0))
        (dolist (file (directory-files directory nil))
          (unless (string-prefix-p "." file)
            (when (or (file-directory-p (expand-file-name file directory)) (grove-extra--valid-extension-p file))
              (cl-incf count))))
        count)
    (funcall orig-fun directory)))

(defun grove-extra-search--glob-args (quote-p)
  (let ((valid-exts (if (listp grove-file-extensions) grove-file-extensions (list grove-file-extensions))))
    (mapconcat (lambda (ext) (if quote-p (format "--glob='*.%s'" ext) (format "--glob=*.%s" ext)))
               valid-exts " ")))

(defun grove-extra-around-search-consult-ripgrep (orig-fun &optional initial)
  (if grove-extra-mode
      (progn
        (require 'consult nil t)
        (let ((consult-ripgrep-args (concat consult-ripgrep-args " " (grove-extra-search--glob-args nil))))
          (consult--grep "Grove search" #'consult--grep-make-builder grove-directory initial)))
    (funcall orig-fun initial)))

(defun grove-extra-around-search-grep (orig-fun &optional initial)
  (if grove-extra-mode
      (let ((pattern (read-string "Grove search: " initial)))
        (grep (format "rg --no-heading --line-number %s %s %s"
                      (grove-extra-search--glob-args t)
                      (shell-quote-argument pattern)
                      (shell-quote-argument grove-directory))))
    (funcall orig-fun initial)))

(defun grove-extra-around-search (orig-fun &optional initial)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (if (or (featurep 'consult) (fboundp 'consult-ripgrep))
            (progn (require 'consult nil t) (grove-search--consult-ripgrep initial))
          (grove-search--grep initial)))
    (funcall orig-fun initial)))

(defun grove-extra-around-search-tag (orig-fun &optional initial)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (let* ((tag (or initial (read-string "Tag: ")))
               (pattern (format "(#%s\\b|:%s:|tags:.*\\b%s\\b)" (regexp-quote tag) (regexp-quote tag) (regexp-quote tag))))
          (if (or (featurep 'consult) (fboundp 'consult-ripgrep))
              (progn
                (require 'consult nil t)
                (let ((consult-ripgrep-args (concat consult-ripgrep-args " " (grove-extra-search--glob-args nil))))
                  (consult--grep "Grove tags" #'consult--grep-make-builder grove-directory pattern)))
            (grep (format "rg --no-heading --line-number %s %s %s"
                          (grove-extra-search--glob-args t)
                          (shell-quote-argument pattern)
                          (shell-quote-argument grove-directory))))))
    (funcall orig-fun initial)))

(defun grove-extra-around-link-follow (orig-fun title)
  (if grove-extra-mode
      (let ((path (grove-link--resolve title)))
        (if path
            (find-file path)
          (if (y-or-n-p (format "Note \"%s\" not found. Create it? " title))
              (let* ((dir-part (file-name-directory title))
                     (file-part (file-name-nondirectory title))
                     (sanitised-file (concat (grove--sanitize-filename file-part) "." grove-default-extension))
                     (rel-path (if dir-part (concat dir-part sanitised-file) sanitised-file))
                     (full-path (expand-file-name rel-path grove-directory)))
                (when dir-part (make-directory (file-name-directory full-path) t))
                (find-file full-path)
                (let ((ext (downcase (file-name-extension full-path))))
                  (if (member ext '("md" "markdown"))
                      (insert "# " file-part "\n\n")
                    (insert "#+title: " file-part "\n\n"))))
            (message "Link not followed"))))
    (funcall orig-fun title)))

(defun grove-extra-around-link-insert (orig-fun)
  (if grove-extra-mode
      (progn
        (grove--refresh-cache)
        (let* ((titles (grove--note-titles))
               (choice (completing-read "Link to: " (mapcar #'car titles) nil nil))
               (alias (read-string "Alias (optional): ")))
          (if (string-empty-p alias)
              (insert "[[" choice "]]")
            (if (derived-mode-p 'markdown-mode)
                (insert "[[" choice "|" alias "]]")
              (insert "[[" choice "][" alias "]]")))))
    (funcall orig-fun)))

(defun grove-extra-around-link-resolve (orig-fun title)
  (if grove-extra-mode
      (progn
        (grove--refresh-cache)
        (let ((matches
               (cl-remove-if-not
                (lambda (pair)
                  (let* ((note-title (car pair))
                         (full-path (cdr pair))
                         (rel-path (file-relative-name full-path grove-directory))
                         (rel-no-ext (file-name-sans-extension rel-path)))
                    (or (string-equal-ignore-case note-title title)
                        (string-equal-ignore-case rel-no-ext title)
                        (string-equal-ignore-case rel-path title))))
                (grove--note-titles))))
          (cond
           ((null matches) nil)
           ((= (length matches) 1) (cdar matches))
           (t (let ((choice (completing-read "Multiple matches. Choose file: "
                                             (mapcar (lambda (m) (file-relative-name (cdr m) grove-directory)) matches)
                                             nil t)))
                (expand-file-name choice grove-directory))))))
    (funcall orig-fun title)))

(defun grove-extra-around-capture (orig-fun)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (let ((buf (get-buffer-create "*grove-capture*")))
          (switch-to-buffer buf)
          (grove-capture-mode 1)
          (let ((inhibit-read-only t)) (erase-buffer))
          (insert "Title\n\nContent")
          (goto-char (point-min))
          (message (substitute-command-keys "Type your note. First line becomes the title. \\[grove-capture-finalize] to save."))))
    (funcall orig-fun)))

(defun grove-extra-around-capture-finalize (orig-fun)
  (if grove-extra-mode
      (progn
        (unless (string= (buffer-name) "*grove-capture*")
          (user-error "Not in the grove capture buffer"))
        (let ((content (string-trim (buffer-string))))
          (if (string-empty-p content)
              (progn
                (when (fboundp 'grove-capture-cancel) (grove-capture-cancel))
                (message "Empty note discarded"))
            (let* ((lines (split-string content "\n"))
                   (title (string-trim (car lines)))
                   (body (string-join (cdr lines) "\n"))
                   (filename (concat (grove--sanitize-filename title) "." grove-default-extension))
                   (path (grove--unique-path (grove--inbox-path) filename)))
              (with-temp-file path
                (if (string= (file-name-extension path) "md")
                    (insert "# " title "\n")
                  (insert "#+title: " title "\n"))
                (unless (string-empty-p body)
                  (insert "\n" body "\n")))
              (kill-buffer (current-buffer))
              (find-file path)
              (message "Note saved: %s" (file-name-nondirectory path))))))
    (funcall orig-fun)))

(defun grove-extra-around-daily (orig-fun &optional time)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (let* ((t-val (or time (current-time)))
               (filename (concat (format-time-string grove-daily-format t-val) "." grove-default-extension))
               (path (expand-file-name filename (grove--daily-path)))
               (new-p (not (file-exists-p path))))
          (find-file path)
          (when new-p
            (if (string= (file-name-extension path) "md")
                (progn
                  (insert "# " (format-time-string "%A, %B %e, %Y" t-val) "\n")
                  (insert "date: " (format-time-string "%F" t-val) "\n\n"))
              (progn
                (insert "#+title: " (format-time-string "%A, %B %e, %Y" t-val) "\n")
                (insert "#+date: " (format-time-string "%F" t-val) "\n\n")))
            (save-buffer))))
    (funcall orig-fun time)))

(defun grove-extra-around-ui-home (orig-fun)
  (if grove-extra-mode
      (let ((daily (expand-file-name (concat (format-time-string grove-daily-format) "." grove-default-extension) (grove--daily-path))))
        (cond
         ((file-exists-p daily) (find-file daily))
         ((> (hash-table-count grove--cache) 0)
          (let* ((all-files (hash-table-keys grove--cache))
                 (first-file (car all-files)))
            (find-file first-file)))
         (t (let ((buf (get-buffer-create "*grove-home*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "Welcome to Grove.\n\nYour vault is empty.\n")
                  (insert "Press 'c' to capture a new note, or 'd' for today's note.")))
              (switch-to-buffer buf)))))
    (funcall orig-fun)))

(defun grove-extra-around-backlink-find (orig-fun title &optional filename)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (unless (executable-find grove-backlink-ripgrep-executable)
          (user-error "Ripgrep not found. Install `%s` and ensure it is on your PATH"
                      grove-backlink-ripgrep-executable))
        (unless filename
          (maphash (lambda (path meta)
                     (when (string-equal-ignore-case (plist-get meta :title) title)
                       (setq filename (file-name-sans-extension (file-name-nondirectory path)))))
                   grove--cache))
        (unless filename (setq filename title))
        
        (let* ((title-pat (regexp-quote title))
               (file-pat (regexp-quote filename))
               (pattern (if (string= title-pat file-pat)
                            (format "\\[\\[(?:[^]]*/)?%s\\]\\]" title-pat)
                          (format "\\[\\[(?:[^]]*/)?(?:%s|%s)\\]\\]" title-pat file-pat)))
               (valid-exts (if (listp grove-file-extensions) grove-file-extensions (list grove-file-extensions)))
               (globs (mapcar (lambda (ext) (format "--glob=*.%s" ext)) valid-exts))
               (args (append (list "--no-heading" "--line-number" "--context" "1")
                             globs (list pattern grove-directory)))
               (ext-re (mapconcat #'grove-extra--make-ci-regexp valid-exts "\\|"))
               results current-file)
          
          (with-temp-buffer
            (let ((exit-code (apply #'process-file grove-backlink-ripgrep-executable nil t nil args)))
              (unless (member exit-code '(0 1 2))
                (user-error "Ripgrep failed with exit code %s" exit-code)))
            (goto-char (point-min))
            (dolist (line (split-string (buffer-string) "\n" t))
              (cond
               ((string-match-p "^--$" line))
               ((string-match (format "^\\(.+\\.\\(?:%s\\)\\):\\([0-9]+\\):\\(.*\\)$" ext-re) line)
                (let ((file (match-string 1 line))
                      (lnum (string-to-number (match-string 2 line)))
                      (context (string-trim (match-string 3 line))))
                  (unless (and current-file (string= file current-file))
                    (push (list :file file :line lnum :context context) results)))))))
          (setq current-file (buffer-file-name))
          (cl-remove-if (lambda (r) (and current-file (string= (plist-get r :file) current-file))) (nreverse results))))
    (funcall orig-fun title filename)))

(defun grove-extra-around-backlinks (orig-fun)
  (if grove-extra-mode
      (progn
        (unless (and (buffer-file-name) (grove-file-p (buffer-file-name)))
          (user-error "Not visiting a grove note"))
        (grove--refresh-cache)
        (let* ((meta (gethash (buffer-file-name) grove--cache))
               (filename (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
               (title (or (plist-get meta :title) filename))
               (results (grove-backlink--find title filename))
               (buf (if (fboundp 'grove-backlink--render)
                        (grove-backlink--render title results)
                      (user-error "grove-backlink--render not found. Check installation."))))
          (display-buffer-in-side-window
           buf
           '((side . bottom)
             (slot . 0)
             (window-height . 12)
             (window-parameters . ((no-delete-other-windows . t)))))
          (message "Found %d backlink(s)" (length results))))
    (funcall orig-fun)))

(defun grove-extra-around-graph-adjacency-list (orig-fun)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (grove--refresh-cache)
        (let ((adjacency (make-hash-table :test #'equal))
              (all-titles (make-hash-table :test #'equal))
              (resolution-map (make-hash-table :test #'equal)))
          (maphash (lambda (path meta)
                     (let* ((title (plist-get meta :title))
                            (rel-path (file-relative-name path grove-directory))
                            (rel-no-ext (file-name-sans-extension rel-path))
                            (filename-no-ext (file-name-sans-extension (file-name-nondirectory path))))
                       (puthash title t all-titles)
                       (puthash (downcase title) title resolution-map)
                       (puthash (downcase rel-path) title resolution-map)
                       (puthash (downcase rel-no-ext) title resolution-map)
                       (puthash (downcase filename-no-ext) title resolution-map)))
                   grove--cache)
          (maphash (lambda (_path meta)
                     (let ((source (plist-get meta :title))
                           (links (plist-get meta :links)))
                       (dolist (raw-target links)
                         (let ((resolved-target (gethash (downcase raw-target) resolution-map)))
                           (when (and resolved-target (gethash resolved-target all-titles))
                             (push resolved-target (gethash source adjacency)))))))
                   grove--cache)
          (let (result)
            (maphash (lambda (title _) (push (cons title (gethash title adjacency)) result)) all-titles)
            result)))
    (funcall orig-fun)))

(defun grove-extra-around-graph (orig-fun)
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (message "Building graph...")
        (let* ((adjacency (grove-graph--adjacency-list))
               (buf (get-buffer-create "*grove-graph*")))
          (with-current-buffer buf
            (grove-graph-mode)
            (when (fboundp 'grove-graph--smil-stop) (grove-graph--smil-stop))
            (setq-local grove-graph--scale (if (eq grove-graph-renderer 'fa2) 1.0 grove-graph-default-zoom))
            (let ((inhibit-read-only t)) (erase-buffer)))
          
          (if (fboundp 'grove-graph--display)
              (grove-graph--display buf)
            (switch-to-buffer buf))
          
          (if (eq grove-graph-renderer 'fa2)
              (grove-graph-fa2-start buf adjacency)
            (let* ((markup (if (eq grove-graph-renderer 'mmdr)
                               (grove-graph--generate-mermaid adjacency)
                             (grove-graph--generate-dot adjacency)))
                   (svg (if (eq grove-graph-renderer 'mmdr)
                            (grove-graph--render-mmdr-svg markup)
                          (grove-graph--render-svg markup))))
              (with-current-buffer buf
                (setq-local grove-graph--raw-svg svg)
                (grove-graph--update-display))
              (message "Graph: %d notes, %d links" (length adjacency)
                       (cl-reduce #'+ (mapcar (lambda (e) (length (cdr e))) adjacency)))))))
    (funcall orig-fun)))

(defun grove-graph--adjust-svg-dimensions (svg-string width height)
  (if (string-match "<svg\\([^>]*?\\)>" svg-string)
      (let* ((attrs (match-string 1 svg-string))
             (clean-attrs (replace-regexp-in-string "[ \t\n\r]*\\(?:width\\|height\\)=\"[^\"]*\"" "" attrs)))
        (replace-match (format "<svg width=\"%d\" height=\"%d\" preserveAspectRatio=\"xMidYMid meet\"%s>" width height clean-attrs) t t svg-string))
    svg-string))

(defun grove-graph--update-display (&rest _)
  (when (and grove-graph--raw-svg (get-buffer-window (current-buffer) t))
    (let* ((inhibit-read-only t)
           (win (get-buffer-window (current-buffer) t))
           (width (max 100 (truncate (* (window-pixel-width win) grove-graph--scale))))
           (height (max 100 (truncate (* (window-pixel-height win) grove-graph--scale))))
           (sized-svg (grove-graph--adjust-svg-dimensions grove-graph--raw-svg width height)))
      (when (= (buffer-size) 0) (insert " "))
      (let* ((max-image-size nil)
             (encoded-svg (if (multibyte-string-p sized-svg) (encode-coding-string sized-svg 'utf-8) sized-svg)))
        (clear-image-cache)
        (put-text-property (point-min) (point-max) 'display (create-image encoded-svg 'svg t)))
      (put-text-property (point-min) (point-max) 'keymap grove-graph-mode-map))))

(defun grove-graph--smil-start ()
  "Starts the animation playback loop if frames are populated."
  (when (or grove-graph--frame-vector grove-graph--frame-offsets)
    (unless grove-graph--smil-timer
      (setq grove-graph--smil-timer (run-with-timer 0 0.016 #'grove-graph--smil-update)))))

(defun grove-graph--smil-stop ()
  "Halts the animation loop."
  (when grove-graph--smil-timer
    (cancel-timer grove-graph--smil-timer)
    (setq grove-graph--smil-timer nil)))

(defun grove-graph--smil-update ()
  "Advances the animation frame natively from either RAM or hidden buffers."
  (let ((graph-buf (get-buffer "*grove-graph*")))
    (when (buffer-live-p graph-buf)
      (with-current-buffer graph-buf
        (let ((total-frames (or (and grove-graph--frame-offsets (length grove-graph--frame-offsets))
                                (and grove-graph--frame-vector (length grove-graph--frame-vector))
                                0)))
          (when (> total-frames 0)
            (if (< grove-graph--current-frame total-frames)
                (progn
                  (let ((bounds (when grove-graph--frame-offsets 
                                  (aref grove-graph--frame-offsets grove-graph--current-frame))))
                    (setq grove-graph--raw-svg
                          (if bounds
                              (with-current-buffer grove-graph--playback-buffer
                                (buffer-substring-no-properties (car bounds) (cdr bounds)))
                            (aref grove-graph--frame-vector grove-graph--current-frame))))
                  (grove-graph--update-display)
                  (cl-incf grove-graph--current-frame))
              (grove-graph--smil-stop))))))))

(defun grove-graph-zoom-in ()
  (interactive)
  (setq grove-graph--scale (* grove-graph--scale 1.2))
  (grove-graph--update-display))

(defun grove-graph-zoom-out ()
  (interactive)
  (setq grove-graph--scale (/ grove-graph--scale 1.2))
  (grove-graph--update-display))

(defun grove-graph-zoom-reset ()
  (interactive)
  (setq grove-graph--scale (if (eq grove-graph-renderer 'fa2) 1.0 grove-graph-default-zoom))
  (grove-graph--update-display))

;;;; ===========================================================================
;;;; GLOBAL MINOR MODE DEFINITION
;;;; ===========================================================================

;;;###autoload
(define-minor-mode grove-extra-mode
  "Global minor mode providing Markdown, FA2, and advanced search tools for Grove."
  :global t
  :group 'grove-extra
  (if grove-extra-mode
      (progn
        ;; 1. Activate Hooks
        (add-hook 'grove-graph-mode-hook #'grove-extra--enable-graph-mode)
        (add-hook 'grove-capture-mode-hook #'grove-extra--enable-capture-mode)
        (add-hook 'find-file-hook #'grove-extra--turn-on-hook)
        
        ;; 2. Apply Advice Wrappers
        (advice-add 'grove--parse-note :around #'grove-extra-around-parse-note)
        (advice-add 'grove--refresh-cache :around #'grove-extra-around-refresh-cache)
        (advice-add 'grove-file-p :around #'grove-extra-around-file-p)
        (advice-add 'grove-search--consult-ripgrep :around #'grove-extra-around-search-consult-ripgrep)
        (advice-add 'grove-search--grep :around #'grove-extra-around-search-grep)
        (advice-add 'grove-search-tag :around #'grove-extra-around-search-tag)
        (advice-add 'grove-link-follow :around #'grove-extra-around-link-follow)
        (advice-add 'grove-link-insert :around #'grove-extra-around-link-insert)
        (advice-add 'grove-link--resolve :around #'grove-extra-around-link-resolve)
        (advice-add 'grove-capture :around #'grove-extra-around-capture)
        (advice-add 'grove-capture-finalize :around #'grove-extra-around-capture-finalize)
        (advice-add 'grove-daily :around #'grove-extra-around-daily)
        (advice-add 'grove-ui-home :around #'grove-extra-around-ui-home)
        (advice-add 'grove-graph--adjacency-list :around #'grove-extra-around-graph-adjacency-list)
        (advice-add 'grove-graph :around #'grove-extra-around-graph)
        (advice-add 'grove-backlink--find :around #'grove-extra-around-backlink-find)
        (advice-add 'grove-backlinks :around #'grove-extra-around-backlinks)
        (advice-add 'grove-tree--list-entries :around #'grove-extra-around-tree-list-entries)
        (advice-add 'grove-tree--item-count :around #'grove-extra-around-tree-item-count)
        (advice-add 'grove-search :around #'grove-extra-around-search)

        (advice-add 'grove-tree-open :after #'grove-extra--lock-sidebar-windows)
        (advice-add 'grove-extra-graph :after #'grove-extra--lock-sidebar-windows))
    (progn
      ;; 1. Deactivate Hooks
      (remove-hook 'grove-graph-mode-hook #'grove-extra--enable-graph-mode)
      (remove-hook 'grove-capture-mode-hook #'grove-extra--enable-capture-mode)
      (remove-hook 'find-file-hook #'grove-extra--turn-on-hook)
      
      ;; 2. Remove Advice Wrappers
      (advice-remove 'grove--parse-note #'grove-extra-around-parse-note)
      (advice-remove 'grove--refresh-cache #'grove-extra-around-refresh-cache)
      (advice-remove 'grove-file-p #'grove-extra-around-file-p)
      (advice-remove 'grove-search--consult-ripgrep #'grove-extra-around-search-consult-ripgrep)
      (advice-remove 'grove-search--grep #'grove-extra-around-search-grep)
      (advice-remove 'grove-search-tag #'grove-extra-around-search-tag)
      (advice-remove 'grove-link-follow #'grove-extra-around-link-follow)
      (advice-remove 'grove-link-insert #'grove-extra-around-link-insert)
      (advice-remove 'grove-link--resolve #'grove-extra-around-link-resolve)
      (advice-remove 'grove-capture #'grove-extra-around-capture)
      (advice-remove 'grove-capture-finalize #'grove-extra-around-capture-finalize)
      (advice-remove 'grove-daily #'grove-extra-around-daily)
      (advice-remove 'grove-ui-home #'grove-extra-around-ui-home)
      (advice-remove 'grove-graph--adjacency-list #'grove-extra-around-graph-adjacency-list)
      (advice-remove 'grove-graph #'grove-extra-around-graph)
      (advice-remove 'grove-backlink--find #'grove-extra-around-backlink-find)
      (advice-remove 'grove-backlinks #'grove-extra-around-backlinks)
      (advice-remove 'grove-tree--list-entries #'grove-extra-around-tree-list-entries)
      (advice-remove 'grove-tree--item-count #'grove-extra-around-tree-item-count)
      (advice-remove 'grove-search #'grove-extra-around-search)
      (advice-remove 'grove-tree-open #'grove-extra--lock-sidebar-windows)
      (advice-remove 'grove-extra-graph #'grove-extra--lock-sidebar-windows))))

(provide 'grove-extra)
;;; grove-extra.el ends here
