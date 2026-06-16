;;; modules/vcs.el --- Version control configuration

(require 'magit)
(require 'forge)

(use-package git-gutter
  :config (global-git-gutter-mode +1))

;; with-editor — use Emacs as $EDITOR in eshell

(require 'with-editor)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)

;; magit-delta — prettier diffs via `delta` binary
;; Requires: brew install git-delta

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))
