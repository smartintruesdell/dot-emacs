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

;; kkp — Kitty Keyboard Protocol
;; In a plain TTY (emacs -nw) the legacy encoding conflates keys: a bare ESC is
;; the same byte as the Meta prefix, C-<letter> variants collapse, etc. On a
;; terminal that speaks the protocol (Ghostty, kitty, foot, WezTerm) kkp makes
;; Emacs read those keys unambiguously — so bare ESC is a clean `escape` event
;; instead of a held Meta prefix. Auto-negotiates per frame; no-op in GUI frames
;; and on terminals without support.

(use-package kkp
  :config
  (global-kkp-mode +1))

;; Ghostty config generation
;; Ghostty (the outer terminal) reads its config at *its* startup, before Emacs
;; runs — so this maintains the file for the *next* Ghostty launch, keeping the
;; single source of truth here in dot-emacs instead of an unsynced dotfile. Only
;; a delimited managed block is owned; other hand-added settings are preserved.
;; Keys prefixed "macos-" are emitted only on macOS, so syncing to Linux stays
;; correct.

(defvar my/ghostty-settings
  '(("macos-option-as-alt" . "left"))
  "Ghostty settings Emacs owns, written into a managed block in the config.
Alist of (KEY . VALUE) strings. Keys starting with \"macos-\" are emitted
only when `system-type' is darwin.")

(defvar my/ghostty-config-file
  (expand-file-name "ghostty/config"
                    (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "Path to the Ghostty config file Emacs maintains.")

(defun my/ghostty--managed-block ()
  "Return the managed-block text for the current platform."
  (let ((lines (seq-remove
                (lambda (kv)
                  (and (string-prefix-p "macos-" (car kv))
                       (not (eq system-type 'darwin))))
                my/ghostty-settings)))
    (concat "# >>> managed by emacs (dot-emacs) — do not edit >>>\n"
            (mapconcat (lambda (kv) (format "%s = %s" (car kv) (cdr kv)))
                       lines "\n")
            "\n# <<< managed by emacs (dot-emacs) <<<")))

(defun my/ghostty-sync-config ()
  "Sync the managed block in the Ghostty config with `my/ghostty-settings'.
Preserves other content, writes only when the file would change, and takes
effect on the next Ghostty launch."
  (let* ((file my/ghostty-config-file)
         (block (my/ghostty--managed-block))
         (existing (when (file-readable-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
         (updated
          (cond
           ((null existing) (concat block "\n"))
           ((string-match "# >>> managed by emacs\\(?:.\\|\n\\)*?# <<< managed by emacs.*$"
                          existing)
            (replace-match block t t existing))
           (t (concat existing (unless (string-suffix-p "\n" existing) "\n")
                      "\n" block "\n")))))
    (unless (equal existing updated)
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert updated))
      (message "Ghostty config synced: %s" file))))

(add-hook 'emacs-startup-hook #'my/ghostty-sync-config)

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
