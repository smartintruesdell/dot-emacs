;;; modules/ui.el --- Visual configuration

;; nano-theme

(require 'nano-splash)
(require 'nano-theme)
(setq nano-fonts-use t)
(setq nano-light-salient "SteelBlue3")
(nano-light)

(defun my/sync-ns-appearance (appearance)
  (set-frame-parameter nil 'ns-appearance appearance)
  (setf (alist-get 'ns-appearance default-frame-alist) appearance))

(advice-add 'nano-dark  :after (lambda (&rest _) (my/sync-ns-appearance 'dark)))
(advice-add 'nano-light :after (lambda (&rest _) (my/sync-ns-appearance 'light)))

;; Fonts

(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'light
                    :height 140)

(set-face-attribute 'bold nil
                    :family "Roboto Mono"
                    :weight 'regular)

(set-fontset-font t 'unicode
                  (font-spec :name "Inconsolata Light" :size 16) nil)

(set-fontset-font t '(#xe000 . #xffdd)
                  (font-spec :name "RobotoMono Nerd Font" :size 12) nil)

;; Modeline

(require 'nano-modeline)
(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
(add-hook 'term-mode-hook            #'nano-modeline-term-mode)
(add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)

;; which-key

(use-package which-key
  :config (which-key-mode))

;; helpful — better *Help* buffers

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;; popper — tame popup buffers

(use-package popper
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*xref\\*"
          "\\*Flymake diagnostics.*\\*"
          "\\*eldoc.*\\*"
          "\\*compilation\\*"
          "\\*cargo.*\\*"
          "\\*Help\\*"
          "\\*helpful.*\\*"
          "\\*Embark.*\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))
