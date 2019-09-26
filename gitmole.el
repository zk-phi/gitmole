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

(defun gitmole--parse-lines (output)
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (let (res)
      (while (search-forward-regexp "^\\([0-9a-f]+\\) " nil t)
        (let* ((revision (match-string 1))
               (author (and (search-forward-regexp "^author \\(.*\\)$")
                            (match-string 1)))
               (time (and (search-forward-regexp "^author-time \\([0-9]+\\)$")
                          (seconds-to-time (string-to-number (match-string 1)))))
               (elapsed-days (floor (/ (float-time (time-since time)) 60 60 24)))
               (message (and (search-forward-regexp "^summary \\(.*\\)$")
                             (match-string 1)))
               (body (and (search-forward-regexp "^\t\\(.*\\)$")
                          (match-string 1)))
               (date-str (format-time-string "%Y/%m/%d" time)))
         (push (vector
                (propertize
                 (concat body "\n")
                 'gitmole-revision revision
                 'gitmole-header (concat (gitmole--abbrev-str revision 8) " " message))
                (propertize
                 (format "%s %-10s" date-str (gitmole--abbrev-str author 10))
                 'face `(:foreground ,(gitmole--make-fg-color elapsed-days) :overline t))
                revision)
               res)))
      (nreverse res))))

(defvar-local gitmole--file-name nil)

;; ---- entrypoint

(defun gitmole--update-header-line-format ()
  (setq header-line-format (or (get-text-property (point) 'gitmole-header) "")))

(defun gitmole-popup-commit ()
  (interactive)
  (let* ((file (or gitmole--file-name
                   (error "Not a gitmole buffer.")))
         (default-directory (file-name-directory file))
         (revision (or (get-text-property (point) 'gitmole-revision)
                       (error "Revision not found."))))
    (with-current-buffer (get-buffer-create "*Git Blame Commit*")
      (read-only-mode 0)
      (erase-buffer)
      (save-excursion
        (insert
         (shell-command-to-string (format "git show --color=always %s -- %s" revision file))))
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode 1))
    (select-window (display-buffer "*Git Blame Commit*"))))

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
           (command (format "git blame --line-porcelain %s -- %s" revision file))
           (parsed-lines (gitmole--parse-lines (shell-command-to-string command)))
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
            default-directory  (file-name-directory file))
      (read-only-mode 1)
      (forward-line lineno)
      (recenter)
      (gitmole--update-header-line-format)
      (add-hook 'post-command-hook 'gitmole--update-header-line-format nil t))))

(provide 'gitmole)
