;;; modules/markdown.el --- Markdown editing + rich preview

;; markdown-mode: use GFM for .md, and let the built-in export/preview
;; commands route through pandoc so tables, fenced code, etc. render.
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  ;; Used by `markdown-export'/`markdown-preview' (the static, one-shot path).
  ;; Requires pandoc on PATH: `brew install pandoc'.
  (setq markdown-command "pandoc --from=gfm --to=html5 --standalone"))

;; grip-mode: instant GitHub-flavored preview with live reload, for both
;; Markdown *and* Org buffers (Org is exported to GFM via ox-gfm first).
;;
;; Backend: `grip-command' defaults to 'auto, picking whichever of these is
;; installed. Recommended (no API rate limits, fully offline):
;;   go-grip -> `go install github.com/chrishrb/go-grip@latest'
;;   mdopen  -> `cargo install mdopen'  (you already have cargo)
;; The original `grip' (pip install grip) works but hits GitHub's API and is
;; rate-limited unless you set `grip-github-user'/`grip-github-password'.
(use-package grip-mode
  :after (markdown-mode org)
  :commands grip-mode
  :init
  (require 'ox-gfm nil t)            ; better Org -> GFM export for previews
  :custom
  (grip-command 'auto)
  (grip-preview-in-webkit nil)       ; use the system browser; set t for xwidget
  (grip-update-after-change t)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)
              :map org-mode-map
              ("C-c C-v g" . grip-mode)))

(provide 'markdown)
;;; markdown.el ends here
