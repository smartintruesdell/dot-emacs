;;; modules/completion.el --- Completion stack

;; corfu — in-buffer completion UI

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.4)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5))

(setq completion-cycle-threshold 3)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq tab-always-indent 'complete)

;; cape — completion-at-point extensions

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; orderless — regexp matching in any order

(use-package orderless
  :custom
  (completion-styles '(substring orderless basic))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

;; vertico — vertical minibuffer completion UI

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-count 8)
  (vertico-count-format nil))

;; marginalia — completion annotations

(use-package marginalia
  :init (marginalia-mode)
  :custom
  (marginalia--ellipsis "…")
  (marginalia-align 'right)
  (marginalia-align-offset -1))

;; consult — completing-read commands

(use-package consult
  :custom (consult-preview-key nil))

;; embark — act on minibuffer candidates

(use-package embark
  :bind (("C-,"   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-c e" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; consult-dir — jump to directories in find-file prompts

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; wgrep — edit grep/ripgrep result buffers inline

(use-package wgrep
  :bind (:map grep-mode-map
         ("C-c C-p" . wgrep-change-to-wgrep-mode))
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))
