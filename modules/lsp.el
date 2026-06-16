;;; modules/lsp.el --- LSP and code intelligence

;; eglot — built-in LSP client

(use-package eglot
  :bind ("C-x ." . eglot-code-actions)
  :config
  ;; TypeScript + TSX: both modes share one language server entry
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode typescript-mode)
                 "typescript-language-server" "--stdio"))
  ;; Rust analyzer with proc-macro and build script support
  (add-to-list 'eglot-server-programs
               `(rust-ts-mode . ("rust-analyzer"
                                 :initializationOptions
                                 (:procMacro (:enable t)
                                  :cargo (:buildScripts (:enable t)
                                          :features "all"))))))

;; eglot-booster — native JSON-RPC parser for 2-3x faster LSP responses
;; Install: cargo install emacs-lsp-booster  (needs Rust: brew install rust)

(use-package eglot-booster
  :after eglot
  :if (executable-find "emacs-lsp-booster")
  :config (eglot-booster-mode))

;; apheleia — format-on-save without blocking

(use-package apheleia
  :config (apheleia-global-mode +1))

;; breadcrumb — current function/class in header line

(use-package breadcrumb
  :hook ((tsx-ts-mode
          typescript-ts-mode
          typescript-mode
          rust-ts-mode) . breadcrumb-local-mode))

;; eldoc-box — float hover docs in a child frame

(use-package eldoc-box
  :bind ("C-c d" . eldoc-box-help-at-point)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-max-pixel-width 500)
  (eldoc-box-max-pixel-height 300))
