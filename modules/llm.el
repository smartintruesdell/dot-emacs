;;; modules/llm.el --- LLM integration (gptel)
;; Requires ANTHROPIC_API_KEY in shell environment (exec-path-from-shell handles this)

(use-package gptel
  :bind (("C-c g"   . gptel)
         ("C-c M-g" . gptel-send)
         ("C-c G"   . gptel-menu))
  :config
  (setq gptel-model "claude-sonnet-4-6"
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (lambda () (getenv "ANTHROPIC_API_KEY"))))
  (setq gptel-default-mode 'org-mode))
