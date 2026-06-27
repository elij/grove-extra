;;; grove-extra.el --- Unofficial extensions for Grove -*- lexical-binding: t -*-

;; Author: Elijah Charles
;; Version: 0.5.1
;; Package-Requires: ((emacs "29.1") (grove "0.1.0"))
;; Description: Adds Markdown support, ForceAtlas2, Mermaid, and SVG scaling to Grove.

(require 'calendar)
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
(require 'graph-fa2)

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

(defcustom grove-extra-use-tab-line 't
  "When non-nil, enable a filtered tab-line showing only grove notes."
  :type 'boolean
  :group 'grove-extra)

(defcustom grove-graph-max-distance nil
  "Maximum distance (in hops) for a local graph render.
If nil, renders the entire graph. If an integer, graph starts at the current
buffer's node and renders up to this many hops."
  :type '(choice (const :tag "Entire graph" nil)
                 (integer :tag "Hops"))
  :group 'grove-extra)

(defcustom grove-graph-tag-groups nil
  "Alist mapping tags to hex colours for graph nodes.
Each element should be of the form (TAG . \"#HEXCODE\").
Nodes with matching tags will be rendered with the specified colour."
  :type '(alist :key-type string :value-type string)
  :group 'grove-extra)

(defvar-local grove-graph--scale 1.0)
(defvar-local grove-graph--raw-svg nil)
(defvar-local grove-extra--hovered-node nil)

(defvar grove-extra--previous-track-mouse nil)
(defvar grove-extra-node-hover-functions nil
  "Hook run when the mouse hovers over a new graph node or leaves a node.
Functions should accept one argument: the NODE-ID string, or nil if empty space.")

(defun grove-extra--tab-line-buffers ()
  "Return a list of grove note buffers for the tab-line, excluding sidebars."
  (cl-remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (and grove-mode
            (not (derived-mode-p 'grove-graph-mode 'grove-tree-mode 'grove-capture-mode))
            (not (string-prefix-p "*" (buffer-name buf))))))
   (buffer-list)))

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
    (grove-mode 1)
    (when grove-extra-use-tab-line
      (setq-local tab-line-tabs-function #'grove-extra--tab-line-buffers)
      (tab-line-mode 1))))

(defun grove-extra--graph-cleanup ()
  "Clean up playback buffers and timers when the graph is closed."
  (when (fboundp 'graph-fa2-player-stop)
    (graph-fa2-player-stop))
  (when (and (boundp 'graph-fa2-playback-buffer)
             (buffer-live-p graph-fa2-playback-buffer))
    (kill-buffer graph-fa2-playback-buffer)))

(define-minor-mode grove-extra-graph-mode
  "Buffer-local minor mode for Grove Graph UI enhancements."
  :init-value nil
  :lighter " Graph+"
  (if grove-extra-graph-mode
      
      (progn
        (setq-local cursor-type nil)
        (setq-local bidi-display-reordering nil)
        (if (not (eq grove-graph-renderer 'fa2))
            (use-local-map grove-graph-mode-map)
          )
        
        (setq-local top-margin-width 0
                    bottom-margin-height 0
                    left-margin-width 0
                    right-margin-width 0)
        (when (get-buffer-window (current-buffer) t)
          (set-window-buffer (get-buffer-window (current-buffer) t) (current-buffer)))

        (setq-local grove-extra--face-cookie 
                    (face-remap-add-relative 'default :background grove-graph-bg-color))
        (add-hook 'kill-buffer-hook #'grove-extra--graph-cleanup nil t)
        (add-hook 'window-size-change-functions #'grove-graph--update-display nil t)
        
        (add-hook 'graph-fa2-node-clicked-functions #'grove-extra--handle-node-clicked nil t))
    (progn
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'bidi-display-reordering)
      
      (kill-local-variable 'top-margin-width)
      (kill-local-variable 'bottom-margin-height)
      (kill-local-variable 'left-margin-width)
      (kill-local-variable 'right-margin-width)
      (when (get-buffer-window (current-buffer) t)
        (set-window-buffer (get-buffer-window (current-buffer) t) (current-buffer)))

      (when (boundp 'grove-extra--face-cookie)
        (face-remap-remove-relative grove-extra--face-cookie))
      (remove-hook 'kill-buffer-hook #'grove-extra--graph-cleanup t)
      (remove-hook 'window-size-change-functions #'grove-graph--update-display t)
      
      (remove-hook 'graph-fa2-node-clicked-functions #'grove-extra--handle-node-clicked t))))

(defun grove-extra--enable-graph-mode ()
  "Turn on extra graph features, interactive bindings, and mouse tracking."
  (when grove-extra-mode
    (grove-extra-graph-mode 1)
    (setq-local track-mouse 'all)))

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

(defun grove-extra-around-file-p (orig-fun file)
  (if grove-extra-mode
      (and grove-directory file
           (grove-extra--valid-extension-p file)
           (string-prefix-p (expand-file-name grove-directory) (expand-file-name file)))
    (funcall orig-fun file)))

(defun grove-extra-around-parse-note (orig-fun file)
  (if grove-extra-mode
      (let ((mtime (file-attribute-modification-time (file-attributes file)))
            (ext (downcase (file-name-extension file)))
            title tags links)
        (with-temp-buffer
          (insert-file-contents file)
          
          (if (member ext '("md" "markdown"))
              (progn
                (markdown-mode)
                (goto-char (point-min))
                (when (fboundp 'markdown-yaml-metadata-alist)
                  (let ((metadata (markdown-yaml-metadata-alist)))
                    (setq title (cdr (assoc "title" metadata)))
                    (let ((raw-tags (cdr (assoc "tags" metadata))))
                      (when raw-tags
                        (setq tags (split-string raw-tags "[:,]\\s-*" t "\\s-*"))))))
                
                (unless title
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\s-+\\(.*\\)" nil t)
                    (setq title (string-trim (match-string 1)))))
                
                (goto-char (point-min))
                (while (re-search-forward "\\(?:\\[\\[\\([^]|]+\\)\\(?:|[^]]+\\)?\\]\\]\\|\\[[^]]*\\](denote:\\([0-9T]+\\))\\)" nil t)
                  (let ((wiki-target (match-string-no-properties 1))
                        (denote-id (match-string-no-properties 2)))
                    (if denote-id
                        (if (fboundp 'denote-get-path-by-id)
                            (let* ((file-path (denote-get-path-by-id denote-id))
                                   (denote-title (when file-path 
                                                   (denote-retrieve-title-value file-path 'markdown))))
                              (push (or denote-title denote-id) links))
                          (push denote-id links))
                      (push wiki-target links)))))
            
            (progn
              (org-mode)
              (let ((keywords (org-collect-keywords '("TITLE" "FILETAGS"))))
                (setq title (cadr (assoc "TITLE" keywords)))
                (let ((raw-tags (cadr (assoc "FILETAGS" keywords))))
                  (when raw-tags
                    (setq tags (split-string raw-tags ":" t "\\s-*")))))

              (org-element-map (org-element-parse-buffer) 'link
                (lambda (link)
                  (let ((type (org-element-property :type link))
                        (path (org-element-property :path link)))
                    (cond
                     ((member type '("fuzzy" "file"))
                      (push path links))
                     
                     ((and (string= type "denote") (fboundp 'denote-get-path-by-id))
                      (let* ((file-path (denote-get-path-by-id path))
                             (denote-title (when file-path 
                                             (denote-retrieve-title-value file-path 'org))))
                        (push (or denote-title path) links)))))))
              
              (unless title
                (setq title (file-name-sans-extension (file-name-nondirectory file))))
              
              (setq tags (grove--merge-tags tags (grove--collect-inline-tags)))
              
              (list :title title :tags tags :links (nreverse links) :mtime mtime)))
          (funcall orig-fun file)))))

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
      (cond
       ((and (fboundp 'denote-link)
             (buffer-file-name)
             (string-prefix-p (expand-file-name (denote-directory)) 
                              (expand-file-name (buffer-file-name))))
        (call-interactively #'denote-link))
       
       ((derived-mode-p 'markdown-mode)
        (grove--refresh-cache)
        (let* ((titles (grove--note-titles))
               (completing-read-function
                (lambda (prompt _collection &rest _args)
                  (completing-read prompt (mapcar #'car titles) nil nil))))
          (if (fboundp 'markdown-insert-wiki-link)
              (call-interactively #'markdown-insert-wiki-link)
            (call-interactively #'markdown-insert-link))))
       
       (t
        (grove--refresh-cache)
        (let* ((titles (grove--note-titles))
               (completing-read-function
                (lambda (prompt _collection &rest _args)
                  (completing-read prompt (mapcar #'car titles) nil nil))))
          (call-interactively #'org-insert-link))))
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
                  (insert "# " (calendar-date-string (calendar-current-date)) "\n")
                  (insert "date: " (format-time-string "%F" t-val) "\n\n"))
              (progn
                (insert "#+title: " (calendar-date-string (calendar-current-date)) "\n")
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
                       
                       (when (stringp title)
                         (puthash title t all-titles)
                         (puthash (downcase title) title resolution-map)
                         (when (stringp rel-path)
                           (puthash (downcase rel-path) title resolution-map)
                           (puthash (downcase rel-no-ext) title resolution-map)
                           (puthash (downcase filename-no-ext) title resolution-map)))))
                   grove--cache)
          
          (maphash (lambda (_path meta)
                     (let ((source (plist-get meta :title))
                           (links (plist-get meta :links)))
                       (dolist (raw-target links)
                         (when (stringp raw-target)
                           (let ((resolved-target (gethash (downcase raw-target) resolution-map)))
                             (when (and resolved-target (gethash resolved-target all-titles))
                               (push resolved-target (gethash source adjacency))))))))
                   grove--cache)
          
          (let (result)
            (maphash (lambda (title _) (push (cons title (gethash title adjacency)) result)) all-titles)
            (let ((max-hops (if (numberp current-prefix-arg) current-prefix-arg grove-graph-max-distance))
                  (current-node nil))
              (when (and max-hops (buffer-file-name) (grove-file-p (buffer-file-name)))
                (let ((meta (gethash (buffer-file-name) grove--cache)))
                  (setq current-node (plist-get meta :title))))
              (if (and max-hops current-node (gethash current-node all-titles))
                  (let ((visited (make-hash-table :test #'equal))
                        (queue (list (cons current-node 0)))
                        filtered-result)
                    (puthash current-node t visited)
                    (while queue
                      (let* ((item (pop queue))
                             (node (car item))
                             (depth (cdr item)))
                        (when (< depth max-hops)
                          (dolist (neighbor (gethash node adjacency))
                            (unless (gethash neighbor visited)
                              (puthash neighbor t visited)
                              (setq queue (append queue (list (cons neighbor (1+ depth)))))))
                          (maphash (lambda (src targets)
                                     (when (and (member node targets)
                                                (not (gethash src visited)))
                                       (puthash src t visited)
                                       (setq queue (append queue (list (cons src (1+ depth)))))))
                                   adjacency))))
                    (maphash (lambda (node _)
                               (let ((targets (cl-remove-if-not (lambda (targ) (gethash targ visited))
                                                                (gethash node adjacency))))
                                 (push (cons node targets) filtered-result)))
                             visited)
                    filtered-result)
                result)))))
    (funcall orig-fun)))

(defun grove-extra--get-node-colour (tags)
  "Determine the node colour based on TAGS and grove-graph-tag-groups."
  (let ((colour "#89b4fa"))
    (when (and (boundp 'grove-graph-tag-groups) grove-graph-tag-groups)
      (catch 'found
        (dolist (group grove-graph-tag-groups)
          (when (member (car group) tags)
            (setq colour (cdr group))
            (throw 'found t)))))
    colour))

(defun grove-extra--prepare-graph-data (adjacency)
  "Iterate over cache and build plain nodes and edges for the graph engine."
  (let ((backlink-counts (make-hash-table :test #'equal))
        (resolution-map (make-hash-table :test #'equal))
        (title-to-meta (make-hash-table :test #'equal))
        (unique-titles (make-hash-table :test #'equal))
        (nodes nil)
        (edges nil))
    (maphash (lambda (path meta)
               (let* ((title (plist-get meta :title))
                      (rel-path (file-relative-name path grove-directory))
                      (rel-no-ext (file-name-sans-extension rel-path))
                      (filename-no-ext (file-name-sans-extension (file-name-nondirectory path))))
                 (when (stringp title)
                   (puthash title meta title-to-meta)
                   (puthash (downcase title) title resolution-map)
                   (when (stringp rel-path)
                     (puthash (downcase rel-path) title resolution-map)
                     (puthash (downcase rel-no-ext) title resolution-map)
                     (puthash (downcase filename-no-ext) title resolution-map)))))
             grove--cache)
    (maphash (lambda (_path meta)
               (let ((links (plist-get meta :links)))
                 (dolist (link links)
                   (when (stringp link)
                     (let ((resolved-title (gethash (downcase link) resolution-map)))
                       (when resolved-title
                         (puthash resolved-title (1+ (gethash resolved-title backlink-counts 0)) backlink-counts)))))))
             grove--cache)
    (dolist (entry adjacency)
      (let ((src (car entry))
            (tgts (cdr entry)))
        (puthash src t unique-titles)
        (dolist (tgt tgts)
          (puthash tgt t unique-titles)
          (push (cons src tgt) edges))))
    (maphash (lambda (title _)
               (let* ((meta (gethash title title-to-meta))
                      (tags (when meta (plist-get meta :tags)))
                      (colour (grove-extra--get-node-colour tags))
                      (bl-count (gethash title backlink-counts 0))
                      (radius (+ 10.0 (* 2.0 bl-count))))
                 (push (list :id title :colour colour :radius radius :label title) nodes)))
             unique-titles)
    (list :nodes (nreverse nodes) :edges (nreverse edges))))

(defun grove-extra--resolve-node-file-with-fallbacks (node-id)
  "Resolve NODE-ID to a file path using a fallback sequence of Denote, Org-roam, and Grove."
  (let (found-file)
    (when (and (fboundp 'denote-directory) (fboundp 'denote-get-path-by-id))
      (let ((files (directory-files-recursively (denote-directory) (regexp-quote node-id))))
        (when files
          (setq found-file (car files)))))
    (when (and (not found-file) (fboundp 'org-roam-db-query))
      (let ((res (org-roam-db-query [:select [file] :from nodes :where (= title $s1)] node-id)))
        (when res
          (setq found-file (caar res)))))
    (unless found-file
      (setq found-file (grove-extra--resolve-node-to-file node-id)))
    found-file))

(defun grove-extra--handle-node-clicked (node-id)
  "Handle a clicked node callback by opening its corresponding file in the main window."
  (let* ((target-file (grove-extra--resolve-node-file-with-fallbacks node-id))
         (graph-win (get-buffer-window "*grove-graph*"))
         (main-win (or (grove-tree--main-window) (and graph-win (next-window graph-win)) (selected-window))))
    (if target-file
        (run-at-time 0 nil
                     (lambda (win file name)
                       (when (window-live-p win)
                         (select-window win))
                       (find-file file)
                       (message "Opened: %s" name))
                     main-win target-file node-id))))

(defun grove-extra-around-graph (orig-fun)
  "Build the graph and dock it into a dedicated sidebar window.
If the animated ForceAtlas2 physics engine is active, prepare the generic data
structures and start the engine."
  (if grove-extra-mode
      (progn
        (grove--ensure-directory)
        (message "Building graph...")
        (let* ((adjacency (grove-graph--adjacency-list))
               (buf (get-buffer-create "*grove-graph*")))
          (with-current-buffer buf
            (grove-graph-mode)
            (grove-extra--enable-graph-mode)
            (when (fboundp 'graph-fa2-player-stop) (graph-fa2-player-stop))
            (setq-local grove-graph--scale (if (eq grove-graph-renderer 'fa2) 1.0 grove-graph-default-zoom))
            (let ((inhibit-read-only t)) (erase-buffer)))
          (let ((win (display-buffer-in-side-window
                      buf
                      `((side . right)
                        (slot . 0)
                        (window-width . 80)
                        (window-parameters . ((no-other-window . nil)
                                              (no-delete-other-windows . t)))))))
            (when win
              (set-window-buffer win buf)
              (window-preserve-size win t t)))
          (if (eq grove-graph-renderer 'fa2)
              (let* ((prepared (grove-extra--prepare-graph-data adjacency))
                     (nodes (plist-get prepared :nodes))
                     (edges (plist-get prepared :edges)))
                (graph-fa2-start buf nodes edges :cache-dir (expand-file-name ".cache" grove-directory)))
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


(defun grove-graph--update-display (&rest _)
  (when (and grove-graph--raw-svg (get-buffer-window (current-buffer) t))
    (let* ((inhibit-read-only t)
           (win (get-buffer-window (current-buffer) t))
           (width (max 100 (truncate (* (window-pixel-width win) grove-graph--scale))))
           (height (max 100 (truncate (* (window-pixel-height win) grove-graph--scale))))
           (sized-svg (grove-graph--adjust-svg-dimensions grove-graph--raw-svg width height)))
      (when (= (buffer-size) 0) (insert " "))
      (let* ((max-image-size nil)
             (encoded-svg (if (multibyte-string-p sized-svg) 
                              (encode-coding-string sized-svg 'utf-8) 
                            sized-svg)))
        (clear-image-cache)
        
        (add-text-properties 
         (point-min) (point-max) 
         (list 'display (create-image encoded-svg 'svg t)
               'pointer (if grove-extra--hovered-node 'hand nil)))))))

(defun grove-graph--adjust-svg-dimensions (svg-string width height)
  (if (string-match "<svg\\([^>]*?\\)>" svg-string)
      (let* ((attrs (match-string 1 svg-string))
             (clean-attrs (replace-regexp-in-string "[ \t\n\r]*\\(?:width\\|height\\)=\"[^\"]*\"" "" attrs)))
        (replace-match (format "<svg width=\"%d\" height=\"%d\" preserveAspectRatio=\"xMidYMid meet\"%s>" width height clean-attrs) t t svg-string))
    svg-string))

(defun grove-extra--resolve-node-to-file (node)
  "Resolve a NODE name to an absolute file path using Grove's internal logic."
  (if (fboundp 'grove-link--resolve)
      (grove-link--resolve node)
    (let ((files (directory-files-recursively grove-directory (format "^%s\\." (regexp-quote node)))))
      (if files
          (car files)
        (expand-file-name (format "%s.md" node) grove-directory)))))

(defvar grove-extra-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Global keymap for grove-extra-mode.")

;;;###autoload
(define-minor-mode grove-extra-mode
  "Global minor mode providing Markdown, FA2, and advanced search tools for Grove."
  :global t
  :group 'grove-extra
  (if grove-extra-mode
      (progn
        (setq grove-extra--previous-track-mouse (default-value 'track-mouse))
        (setq-default track-mouse t)
        
        (add-hook 'grove-graph-mode-hook #'grove-extra--enable-graph-mode)
        (add-hook 'grove-capture-mode-hook #'grove-extra--enable-capture-mode)
        (add-hook 'find-file-hook #'grove-extra--turn-on-hook)
        
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
        (advice-add 'grove-graph :after #'grove-extra--lock-sidebar-windows))
    (progn
      (setq-default track-mouse grove-extra--previous-track-mouse)
      
      (remove-hook 'grove-graph-mode-hook #'grove-extra--enable-graph-mode)
      (remove-hook 'grove-capture-mode-hook #'grove-extra--enable-capture-mode)
      (remove-hook 'find-file-hook #'grove-extra--turn-on-hook)
      
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
      (advice-remove 'grove-graph #'grove-extra--lock-sidebar-windows))))

(provide 'grove-extra)
;;; grove-extra.el ends here
