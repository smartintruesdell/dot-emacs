;; config.el — Emacs configuration bootstrap
;; Shawn Truesdell
;;
;; Bootstraps straight.el, installs all packages, and loads modules.
;; Package configuration lives in modules/*.el, not here.

;; --- straight.el bootstrap ---

(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Core macros — must be available before use-package invocations in modules
(straight-use-package 'use-package)
(straight-use-package 'bind-key)
(require 'bind-key)

;; --- Package installation ---

(dolist (package '(;; Completion
                   cape
                   orderless
                   vertico
                   marginalia
                   consult
                   consult-dir
                   consult-recoll
                   corfu
                   embark
                   embark-consult
                   wgrep
                   ;; Editing
                   avy
                   jinx
                   move-text
                   smartparens
                   hungry-delete
                   olivetti
                   switch-window
                   ;; Utilities
                   f
                   guess-language
                   helpful
                   htmlize
                   mini-frame
                   which-key
                   ;; VCS
                   magit
                   forge
                   git-gutter
                   magit-delta
                   ;; Notes / Zettelkasten (Denote — DB-free)
                   denote
                   denote-org
                   denote-journal
                   denote-sequence
                   consult-denote
                   denote-explore
                   ;; File formats
                   markdown-mode
                   yaml-mode
                   grip-mode
                   ox-gfm
                   ;; Infrastructure
                   use-package
                   bind-key
                   vc-backup
                   exec-path-from-shell
                   no-littering
                   ;; UI
                   nano-modeline
                   all-the-icons
                   all-the-icons-dired
                   dired-preview
                   popper
                   ;; Languages / LSP
                   typescript-mode
                   web-mode
                   treesit-auto
                   apheleia
                   eglot
                   breadcrumb
                   eldoc-box
                   yasnippet
                   yasnippet-snippets
                   cargo
                   ;; LLM
                   gptel
                   ;; Terminal
                   vterm
                   kkp
                   ;; Editing utilities
                   string-inflection
                   vundo
                   ;; Linting
                   flymake-eslint))
  (straight-use-package package))

;; Git-hosted packages without MELPA recipes
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))
(straight-use-package
 '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
(straight-use-package
 '(nano-splash :type git :host github :repo "rougier/nano-splash"))
(straight-use-package
 '(nano-theme :type git :host github :repo "rougier/nano-theme"))
(straight-use-package
 '(eglot-booster :type git :host github :repo "jdtsmith/eglot-booster"))
(straight-use-package
 '(combobulate :type git :host github :repo "mickeynp/combobulate"))
;; ghostel — pinned to its git repo (mirrors the MELPA recipe's :files, incl. the
;; src/vendor/build.zig the native-module installer needs) because older straight
;; recipe caches predate ghostel's MELPA addition.
(straight-use-package
 '(ghostel :type git :host github :repo "dakra/ghostel"
           :files (:defaults "etc" "src" "vendor" "build.zig" "build.zig.zon" "symbols.map")))

;; --- Shell PATH (must run early so later packages can find binaries) ---

(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH" "ANTHROPIC_API_KEY"))
(exec-path-from-shell-initialize)

;; --- no-littering (must load before other packages scatter files) ---

(require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;; --- Module loader ---

(defun my/load-module (name)
  (load (expand-file-name (format "modules/%s" name) user-emacs-directory)
        nil 'nomessage))

(my/load-module "core")
(my/load-module "ui")
(my/load-module "completion")
(my/load-module "editing")
(my/load-module "dired")
(my/load-module "vcs")
(my/load-module "lsp")
(my/load-module "languages")
(my/load-module "org")
(my/load-module "markdown")
(my/load-module "denote")
(my/load-module "llm")
(my/load-module "keybindings")
