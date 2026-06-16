;;; modules/languages.el --- Language-specific configuration

;; treesit-auto — auto-install grammars and alias major modes

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; TypeScript / TSX

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx?" . tsx-ts-mode)))

;; tsi.el — tree-sitter-based indentation

(require 'tsi)
(add-hook 'tsx-ts-mode-hook     (lambda () (tsi-typescript-mode 1)))
(add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
(add-hook 'json-mode-hook       (lambda () (tsi-json-mode 1)))
(add-hook 'css-mode-hook        (lambda () (tsi-css-mode 1)))
(add-hook 'scss-mode-hook       (lambda () (tsi-scss-mode 1)))

;; eglot-ensure hooks (server starts automatically when visiting these files)

(add-hook 'tsx-ts-mode-hook        'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)

;; Rust

(add-to-list 'auto-mode-alist '("\\.rs" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook 'cargo-minor-mode)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)

;; combobulate — structural editing and navigation via tree-sitter

(use-package combobulate
  :hook ((tsx-ts-mode        . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (rust-ts-mode       . combobulate-mode)))

;; flymake-eslint — ESLint diagnostics in the same flymake system eglot uses

(use-package flymake-eslint
  :hook ((tsx-ts-mode        . flymake-eslint-enable)
         (typescript-ts-mode . flymake-eslint-enable)))

;; Snippets

(use-package yasnippet
  :config (yas-global-mode))
