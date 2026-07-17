;;; modules/editing.el --- Text editing configuration

;; move-text — move lines up/down

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

;; smartparens

(use-package smartparens
  :custom (sp-escape-quotes-after-insert nil)
  :config (smartparens-global-mode 1))

;; hungry-delete — delete all whitespace in one keystroke

(use-package hungry-delete
  :config (global-hungry-delete-mode))

;; avy — jump to any visible text

(use-package avy
  :bind ("M-j" . avy-goto-char-timer))

;; jinx — fast enchant-based spell checker (replaces flyspell stack)
;; Requires: brew install enchant

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (add-hook 'jinx-mode-hook
            (lambda ()
              (when (fboundp 'guess-language-mode)
                (guess-language-mode 1)))))

;; olivetti — distraction-free writing margins

(use-package olivetti)

;; switch-window — visual window selection

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

;; Window splitting helpers — focus new window after split

(defun my/hsplit-last-buffer ()
  "Split window horizontally and focus the new window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun my/vsplit-last-buffer ()
  "Split window vertically and focus the new window."
  (interactive)
  (split-window-vertically)
  (other-window 1))

;; UUID insertion

(defun my/insert-random-uuid ()
  "Insert a UUID. Calls uuidgen on MacOS/Linux, PowerShell on Windows."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((eq system-type 'darwin)
    (shell-command "uuidgen" t))
   ((eq system-type 'gnu/linux)
    (shell-command "uuidgen" t))
   (t
    (let ((xstr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                             (user-uid) (emacs-pid) (system-name)
                             (user-full-name) (current-time) (emacs-uptime)
                             (garbage-collect) (buffer-string)
                             (random) (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring xstr 0 8)
                      (substring xstr 8 12)
                      (substring xstr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring xstr 17 20)
                      (substring xstr 20 32)))))))

;; vterm — full-featured terminal emulator (compiles a C module on first load)

;; ghostel — primary terminal (C-c t). libghostty-vt forwards the Kitty keyboard
;; protocol and SGR mouse *to child programs*, so nested TUIs (agent-deck, Claude
;; Code) get bare ESC, Shift+Enter, and mouse-wheel scroll natively — no vterm
;; workarounds needed. The native module downloads automatically on first launch.

(use-package ghostel
  :bind ("C-c t" . ghostel))

;; vterm — kept as a fallback on C-c T. Faster raw throughput, but can't forward
;; the Kitty keyboard protocol/mouse to children (hence the C-c C-e escape helper).

(use-package vterm
  :bind (("C-c T" . vterm)
         :map vterm-mode-map
         ("C-c C-e" . vterm-send-escape))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t))

;; string-inflection — cycle identifier case: camelCase → snake_case → PascalCase → SCREAMING_SNAKE_CASE

(use-package string-inflection
  :bind ("C-c i" . string-inflection-cycle))

;; vundo — navigate Emacs's non-linear undo history as a visual tree

(use-package vundo
  :bind ("C-c u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;; Trailing whitespace

(add-hook 'before-save-hook 'delete-trailing-whitespace)
