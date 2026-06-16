;;; modules/dired.el --- Dired configuration

(use-package dired
  :straight nil
  :ensure nil
  :bind (:map dired-mode-map ("M-+" . dired-create-empty-file))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always)
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(require 'all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; dired-preview — preview file under cursor in a split window

(use-package dired-preview
  :bind (:map dired-mode-map ("C-c p" . dired-preview-mode))
  :custom
  (dired-preview-delay 0.3)
  (dired-preview-max-size (* 10 1024 1024))
  (dired-preview-ignored-extensions-regexp
   (concat "\\."
           (regexp-opt '("mkv" "webm" "mp4" "mp3" "ogg" "m4a"
                         "gz" "zst" "tar" "xz" "rar" "zip") t)
           "\\'")))

;; Concatenate marked dired files into a new buffer

(defun dired-concatenate-marked-files ()
  "Concatenate marked files into a new buffer, each separated by a header comment."
  (interactive)
  (let ((files (dired-get-marked-files))
        (buffer-name "*Concatenated Files*")
        (line-width 80))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (dolist (file files)
        (goto-char (point-max))
        (let* ((filename (format "// %s " (file-name-nondirectory file)))
               (padding (make-string (- line-width (length filename)) ?*)))
          (insert (concat filename padding "\n")))
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "\n"))
      (goto-char (point-min)))
    (switch-to-buffer buffer-name)))
