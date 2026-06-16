;;; modules/core.el --- Core Emacs settings

;; Compatibility shim: nano-modeline uses `incf` from the old `cl` library
(unless (fboundp 'incf)
  (defalias 'incf #'cl-incf))

;; Startup

(setq-default
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message ""
 initial-buffer-choice t)

;; Encoding

(set-default-coding-systems 'utf-8)
(prefer-coding-system       'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "English")

;; Backups
;; Kept with explicit path for vc-backup compatibility (overrides no-littering's default)

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files t
      vc-make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 6
      kept-new-versions 9
      delete-by-moving-to-trash t)

(require 'vc-backup)

;; Defaults

(setq-default
 ad-redefinition-action 'accept
 cursor-in-non-selected-windows t
 display-time-default-load-average nil
 fill-column 80
 help-window-select t
 indent-tabs-mode nil
 kill-ring-max 128
 load-prefer-newer t
 mark-ring-max 128
 scroll-conservatively most-positive-fixnum
 select-enable-clipboard t
 tab-width 2
 user-full-name "Shawn Martin-Truesdell"
 user-mail-address "shawn@martin-truesdell.com"
 vc-follow-symlinks t
 view-read-only t)

(cd "~/")

(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode)
(show-paren-mode 1)
(setq ring-bell-function 'ignore)
(global-unset-key (kbd "C-z"))

;; History

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(require 'savehist)

(setq kill-ring-max 50
      history-length 50
      history-delete-duplicates t)

(setq savehist-additional-variables
      '(kill-ring
        command-history
        set-variable-value-history
        custom-variable-history
        query-replace-history
        read-expression-history
        minibuffer-history
        read-char-history
        face-name-history
        bookmark-history
        file-name-history))

(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)

(let (message-log-max)
  (savehist-mode))

(save-place-mode 1)

;; OS-specific settings

(cond
 ((string-equal system-type "windows-nt")
  (message "Microsoft Windows"))
 ((string-equal system-type "darwin")
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'option)
  (message "Mac OS X"))
 ((string-equal system-type "gnu/linux")
  (message "Linux")))

;; UI chrome

(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; ANSI color in compilation buffers

(require 'ansi-color)

(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'endless/colorize-compilation)
