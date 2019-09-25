(require 'color)
(require 'cl-lib)
(require 'subr-x)

(defconst gitmole-fg-color-saturation 0.5)

(defconst gitmole-fg-color-luminance
  (+ (cl-caddr (apply 'color-rgb-to-hsl (color-name-to-rgb (face-foreground 'default))))
     (if (eq frame-background-mode 'light) 0.05 -0.05)))

(defconst gitmole-fg-color-hue-from 0.5)
(defconst gitmole-fg-color-hue-to 0.0)

(defconst gitmole-fg-color-max-days (* 3 360))

;; ---- utilities

(defun gitmole--make-fg-color (days)
  (let ((fraction (min (/ days 1.0 gitmole-fg-color-max-days) 1)))
    (apply 'color-rgb-to-hex
           (color-hsl-to-rgb
            (+ (* (- 1 fraction) gitmole-fg-color-hue-from)
               (* fraction gitmole-fg-color-hue-to))
            gitmole-fg-color-saturation
            gitmole-fg-color-luminance))))

(defun gitmole--abbrev-str (str len)
  "char-width sensitive string shortener"
  (let ((chars (string-to-list str))
        (width 0)
        lst)
    (while (and chars (<= (+ width (char-width (car chars))) len))
      (push (car chars) lst)
      (cl-incf width (char-width (pop chars))))
    (apply 'string (nreverse lst))))

;; ---- parse git-blame output

(defconst gitmole--blame-format-regexp
  "^\\(\\^?\\)\\([0-9a-f]+\\) \\(?:[^(]+ \\)?(\\(.*[^ ]\\) +\\([-0-9]+\\) \\([:0-9]+ [-+][0-9]+\\) +\\([0-9]+\\)) \\(.*\\)$")

(defun gitmole--parse-line (line)
  (unless (string-match gitmole--blame-format-regexp line)
    (error "Unexpected error parsing \"%s\"." line))
  (let* ((object-id (match-string 2 line))
         ;; (is-initial (match-string 1 line))
         (body (match-string 7 line))
         ;; (lineno (match-string 6 line))
         (username (match-string 3 line))
         (date (match-string 4 line))
         (time (match-string 5 line))
         (elapsed-days (floor (/ (float-time (time-since (concat date " " time))) 60 60 24))))
    (vector (propertize
             (concat body "\n")
             'gitmole-revision object-id)
            (propertize
             (format "%s %-10s" date (gitmole--abbrev-str username 10))
             'face `(:foreground ,(gitmole--make-fg-color elapsed-days) :overline t))
            object-id)))

(defvar-local gitmole--file-name nil)

;; ---- entrypoint

(defun gitmole-interactive-blame (&optional revision)
  (interactive)
  (let ((file (or gitmole--file-name (buffer-file-name)))
        (lineno (line-number-at-pos nil t)))
    (unless file
      (error "Not a file buffer."))
    (unless (locate-dominating-file file ".git")
      (error "Not under a git repo."))
    (let* ((default-directory (file-name-directory file))
           (revision (or revision (get-text-property (point) 'gitmole-revision)))
           (revision (if revision (concat revision "^") "HEAD"))
           (command (format "git blame %s -- %s" revision file))
           (lines (split-string (shell-command-to-string command) "\n" t))
           (parsed-lines (mapcar 'gitmole--parse-line lines))
           (header (shell-command-to-string (format "git log --oneline %s^..%s" revision revision)))
           last-revision)
      (switch-to-buffer (get-buffer-create "*Git Blame*"))
      (read-only-mode 0)
      (erase-buffer)
      (remove-overlays (point) (point-max))
      (dolist (line parsed-lines)
        (insert (aref line 0)))
      (let ((buffer-file-name file))
        (set-auto-mode))
      (goto-char (point-min))
      (save-excursion
        (dolist (line parsed-lines)
          (overlay-put
           (make-overlay (point) (point))
           'before-string
           (if (string= (aref line 2) last-revision)
               "                     "
             (setq last-revision (aref line 2))
             (aref line 1)))
          (forward-line 1)))
      (setq gitmole--file-name file
            default-directory     (file-name-directory file)
            header-line-format    (string-trim header))
      (read-only-mode 1)
      (forward-line lineno)
      (recenter))))

(provide 'gitmole)
