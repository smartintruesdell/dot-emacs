;; config.el
;; The Emacs configuration of Shawn Truesdell

;; Package Management - Straight.el
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

;; (straight-pull-recipe-repositories) ;; only necessary if recipes are missing.

;; Packages installation

(setq package-list
      '(cape                ; Completion At Point Extensions
        orderless           ; Completion style for matching regexps in any order
        vertico             ; VERTical Interactive COmpletion
        marginalia          ; Enrich existing commands with completion annotations
        avy                 ; Cursor/Pointer interaction FSA library
        consult             ; Consulting completing-read
        corfu               ; Completion Overlay Region FUnction
        deadgrep            ; superfast grep
        bind-key            ; easy keybindings
        move-text           ; easy up/down moving
        ;; deft                ; Quickly browse, filter, and edit plain text notes
        f                   ; Modern API for working with files and directories
        ;; citar               ; Citation-related commands for org, latex, markdown
        ;; citeproc            ; A CSL 1.0.2 Citation Processor
        flyspell-correct-popup ; Correcting words with flyspell via popup interface
        flyspell-popup      ; Correcting words with Flyspell in popup menus
        guess-language      ; Robust automatic language detection
        helpful             ; A better help buffer
        htmlize             ; Convert buffer text and decorations to HTML
        mini-frame          ; Show minibuffer in child frame on read-from-minibuffer
        olivetti            ; Smart margins for better writing
        ;; imenu-list          ; Show imenu entries in a separate buffer
        magit               ; A Git porcelain inside Emacs.
	      forge               ; Forge (GitHub, GitLab, etc) integrations for Magit
        git-gutter          ; change markers in the gutter
        markdown-mode       ; Major mode for Markdown-formatted text
        ;; multi-term          ; Managing multiple terminal buffers in Emacs.D
        ;; pinentry            ; GnuPG Pinentry server implementation
        use-package         ; A configuration macro for simplifying your .emacs
        vc-backup           ; VC backend for versioned backups
        yaml-mode           ; YAML mode
        consult-recoll      ; Consult interface for recoll query
        ;; org-auto-tangle     ; Tangle org file when it is saved
        exec-path-from-shell; Get environment variables such as $PATH from the shell
        nano-modeline       ; NANO customized modeline
        tabnine             ; Awesome AI powered autocompletion
        typescript-mode     ; TypeScript
        web-mode            ; JSX-enabled web mode
        apheleia            ; Format-on-Save for code buffers
        eglot               ; language-server backing for Emacs
        yasnippet           ; Awesome snippets
        yasnippet-snippets  ; Presets
        cargo               ; Rust cargo minor-mode
        smartparens         ; Only somewhat smart
        hungry-delete       ; eat all the whitespace
        switch-window       ; smart window switching
        all-the-icons       ; cute icons
        all-the-icons-dired ; cute icons, dired edition
        which-key))         ; Display available keybindings in popup

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH"))
(exec-path-from-shell-initialize)

;; Special case for pdf-tools that has recently (2022) changed maintainer
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

(straight-use-package
 '(tsi :type git :host github :repo "orzechowskid/tsi.el"))

(require 'bind-key)

;; TabNine

;; === BEGIN TABNINE KEY ===
;; shawntruesdell@gmail.com
;; fpctaeuujatzulocpebuumbdfkbbfqaj
;; === END TABNINE KEY ===

(use-package tabnine
  :commands (tabnine-start-process)
  :hook (prog-mode . tabnine-mode)
  :diminish "⌬"
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 0)
  :hook (kill-emacs . tabnine-kill-process)
  :config

  (tabnine-start-process)
  :bind
  (:map tabnine-completion-map
	      ("<tab>" . tabnine-accept-completion)
	      ("TAB" . tabnine-accept-completion)
	      ("M-f" . tabnine-accept-completion-by-word)
	      ("M-<return>" . tabnine-accept-completion-by-line)
	      ("C-g" . tabnine-clear-overlay)
	      ("M-[" . tabnine-previous-completion)
	      ("M-]" . tabnine-next-completion)))

;; NANO modules

;; - NANO splash
(straight-use-package
 '(nano-splash :type git :host github :repo "rougier/nano-splash"))

(require 'nano-splash)

(straight-use-package
 '(nano-theme :type git :host github :repo "rougier/nano-theme"))

(require 'nano-theme)
(setq nano-fonts-use t) ; Use theme font stack
(setq nano-light-salient "SteelBlue3")
(nano-light)
;; As a note, to get the titlebar to theme correctly on MacOS you should do
;; $ defaults write org.gnu.Emacs TransparentTitleBar DARK

(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'light
                    :height 140)

(set-face-attribute 'bold nil
                    :family "Roboto Mono"
                    :weight 'regular)

;; TODO: There's an extra step to hack the font to fit. :(
;; (set-face-attribute 'italic nil
;;                     :family "Victor Mono"
;;                     :weight 'semilight
;;                     :slant 'italic)

(set-fontset-font t 'unicode
                  (font-spec :name "Inconsolata Light"
                             :size 16) nil)

(set-fontset-font t '(#xe000 . #xffdd)
                  (font-spec :name "RobotoMono Nerd Font"
                             :size 12) nil)

;; Completion
(require 'corfu)

(setq corfu-cycle t                ; Enable cycling for `corfu-next/previous'
      corfu-auto t                 ; Enable auto completion
      corfu-auto-delay 60.0        ; Delay before auto-completion shows up
      corfu-separator ?\s          ; Orderless field separator
      corfu-quit-at-boundary nil   ; Never quit at completion boundary
      corfu-quit-no-match t        ; Quit when no match
      corfu-preview-current nil    ; Disable current candidate preview
      corfu-preselect-first nil    ; Disable candidate preselection
      corfu-on-exact-match nil     ; Configure handling of exact matches
      corfu-echo-documentation nil ; Disable documentation in the echo area
      corfu-scroll-margin 5)       ; Use scroll margin

(global-corfu-mode)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; completion-at-point is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Completion in source blocks
(require 'cape)

(add-to-list 'completion-at-point-functions 'cape-symbol)
(add-to-list 'completion-at-point-functions 'tabnine-completion-at-point)

(require 'orderless)

(setq completion-styles '(substring orderless basic)
      orderless-component-separator 'orderless-escapable-split-on-space
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(require 'consult)

(setq consult-preview-key nil) ; No live preview

(bind-key "C-x C-r" #'consult-recent-file)
(bind-key "C-x h"   #'consult-outline)
(bind-key "C-x b"   #'consult-buffer)
(bind-key "C-c h"   #'consult-history)

(require 'vertico)

;; (setq completion-styles '(basic substring partial-completion flex))

(setq vertico-resize nil        ; How to resize the Vertico minibuffer window.
      vertico-count 8           ; Maximal number of candidates to show.
      vertico-count-format nil) ; No prefix with number of entries

(vertico-mode)

(require 'marginalia)

(setq-default marginalia--ellipsis "…"    ; Nicer ellipsis
              marginalia-align 'right     ; right alignment
              marginalia-align-offset -1) ; one space on the right

(marginalia-mode)

;; Startup

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t)               ; Open *scratch* buffer at init

;; Encoding

(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

;; Recovery & Backups

(setq auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
      (expand-file-name ".auto-save-list/.saves-" user-emacs-directory)
      auto-save-default t        ; Auto-save every buffer that visits a file
      auto-save-timeout 20       ; Number of seconds between auto-save
      auto-save-interval 200)    ; Number of keystrokes between auto-saves

(setq backup-directory-alist       ; File name patterns and backup directory names.
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files t          ; Backup of a file the first time it is saved.
      vc-make-backup-files t       ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 6          ; Number of old versions to keep
      kept-new-versions 9          ; Number of new versions to keep
      delete-by-moving-to-trash t) ; Delete files to trash

;; - Back
(require 'vc-backup)

;; Bookmarks

(setq bookmark-default-file (expand-file-name "bookmark" user-emacs-directory))

;; Defaults

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 display-time-default-load-average nil            ; Don't display load average
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when
 indent-tabs-mode nil                             ; Use spaces instead of tabs
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 kill-ring-max 128                                ; Maximum length of kill ring
 load-prefer-newer t                              ; Prefers the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 2                                      ; Set width for tabs
 user-full-name "Shawn Martin-Truesdell"          ; Set the full name of the current user
 user-mail-address "shawn@martin-truesdell.com"   ; Set the email address of the current user
 vc-follow-symlinks t                             ; Always follow the symlinks
 view-read-only t)                                ; Always open read-only buffers in view-mode

(cd "~/")                                         ; Move to the user directory

(column-number-mode 1)                            ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode)                             ; Hightlight current line
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent
(setq ring-bell-function 'ignore)                 ; No more dings
(global-unset-key (kbd "C-z"))                    ; No more minimizing on accident

;; History

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(require 'savehist)

(setq kill-ring-max 50
      history-length 50)

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

(setq history-delete-duplicates t)

(let (message-log-max)
  (savehist-mode))

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-forget-unreadable-files t)

(save-place-mode 1)

;; Custom

(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file nil t))

;; Fix meta =M-x= is a chord we use ALL THE TIME, and having it right
;; under the center of our left palm is super bad for our
;; ergonomics. Instead, we're going to bind a new chord to make it
;; more convenient.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; OS Specific customizations
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    ;; Macbook Keyboard
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'option)
    ;; Mechanical Keyboard
    ;; (setq mac-option-key-is-meta t)
    ;; (setq mac-command-key-is-meta nil)
    ;; (setq mac-command-modifier 'option)
    ;; (setq mac-option-modifier 'meta)
    (message "Mac OS X")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux"))))

(when window-system
  (menu-bar-mode -1)              ; Disable the menu bar
  (scroll-bar-mode -1)            ; Disable the scroll bar
  (tool-bar-mode -1)              ; Disable the tool bar
  (tooltip-mode -1))              ; Disable the tooltips

;; Software Engineering

(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx?" . tsx-ts-mode))

;; - Syntax Highlighting
(require 'treesit)

(global-tree-sitter-mode)
(add-to-list 'treesit-language-source-alist
             '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
(add-to-list 'treesit-language-source-alist
             '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;; (setq treesit-load-name-override-list '((tsx "libtree-sitter-tsx" "tree_sitter_typescript")))
(add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . typescript))

(when
    (not (treesit-language-available-p 'typescript))
  (treesit-install-language-grammar 'typescript))
(when
    (not (treesit-language-available-p 'tsx))
  (treesit-install-language-grammar 'tsx))

(require 'tsi)
(add-hook 'tsx-ts-mode-hook (lambda () (tsi-typescript-mode 1)))
(add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
(add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
(add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
(add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1)))

(require 'eglot)
(add-to-list 'eglot-server-programs
             '((typescript-mode) "typescript-language-server" "--stdio")
             '((tsx-ts-mode) "typescript-language-server" "--stdio"))
(add-to-list 'eglot-server-programs
             `(rust-ts-mode . ("rust-analyzer" :initializationOptions
                               ( :procMacro (:enable t)
                                 :cargo ( :buildScripts (:enable t)
                                          :features "all")))))

(bind-key "C-x ." 'eglot-code-actions)

(require 'apheleia)
(apheleia-global-mode +1)

(require 'deadgrep)
(keymap-global-set "<f5>" 'deadgrep)

(use-package dired
  :straight nil
  :ensure nil
  :bind (:map dired-mode-map ("M-+" . dired-create-empty-file))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always)
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode))))

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))


(require 'all-the-icons)
(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package smartparens
  :custom (sp-escape-quotes-after-insert nil)
  :config (smartparens-global-mode 1))

(use-package hungry-delete :config (global-hungry-delete-mode))

(defun my/hsplit-last-buffer ()
  "Gives the focus to the last created horizontal window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))
(defun my/vsplit-last-buffer ()
  "Gives the focus to the last created vertical window."
  (interactive)
  (split-window-vertically)
  (other-window 1))
(bind-key "C-x 3" 'my/hsplit-last-buffer)
(bind-key "C-x 2" 'my/vsplit-last-buffer)

(use-package switch-window
  :straight nil
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'magit)
(require 'forge)
(require 'git-gutter)
(global-git-gutter-mode +1)

(yas-global-mode)

(bind-key "C-w" 'backward-kill-word)
(bind-key "C-x C-k" 'kill-region)
(bind-key "C-c C-k" 'kill-region)

;; This causes Emacs to crash when restoring from the background, somehow
;;(setq-default frame-title-format '("Shawn's Emacs :: %b"))

(add-to-list 'auto-mode-alist '("\\.rs" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook 'cargo-minor-mode)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)

(defun my/insert-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://xahlee.info/emacs/emacs/elisp_generate_uuid.html'
Version: 2020-06-04 2023-05-13"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((eq system-type 'darwin) ; Mac
    (shell-command "uuidgen" t))
   ((eq system-type 'gnu/linux)
    (shell-command "uuidgen" t))
   (t
    ;; code here by Christopher Wellons, 2011-11-18.
    ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    (let ((xstr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                             (user-uid)
                             (emacs-pid)
                             (system-name)
                             (user-full-name)
                             (current-time)
                             (emacs-uptime)
                             (garbage-collect)
                             (buffer-string)
                             (random)
                             (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring xstr 0 8)
                      (substring xstr 8 12)
                      (substring xstr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring xstr 17 20)
                      (substring xstr 20 32)))))))

;; Add ansi-color support to the compilation buffer.
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(defun dired-concatenate-marked-files ()
  "Concatenate marked files in Dired into a new buffer with each file separated by a comment, padded with * to a width of 80 characters."
  (interactive)
  (let ((files (dired-get-marked-files))  ;; Get the marked files
        (buffer-name "*Concatenated Files*")
        (line-width 80))  ;; Fixed width for the comment line
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)  ;; Clear buffer before inserting anything
      (dolist (file files)
        (goto-char (point-max))  ;; Move to the end of the buffer
        ;; Prepare the comment line with filename, followed by * to fill up to 80 characters
        (let* ((filename (format "// %s " (file-name-nondirectory file)))
               (padding (make-string (- line-width (length filename)) ?*)))  ;; Create padding
          (insert (concat filename padding "\n")))  ;; Insert the padded filename comment
        ;; Insert the contents of the file, followed by a single newline
        (insert-file-contents file)
        (goto-char (point-max))  ;; Move to the end after inserting contents
        (insert "\n"))  ;; Insert a newline after the file contents
      (goto-char (point-min)))  ;; Move point to the beginning of the buffer
    (switch-to-buffer buffer-name)))  ;; Show the buffer to the user

(require 'org)
(require 'project)

(defun my/project-org-file ()
  "Return the path to the project's tasks.org file, creating it if necessary."
  (let* ((project (or (project-current t)
                      (error "Not in a project")))
         (project-root (project-root project)))
    (let ((org-file (expand-file-name "tasks.org" project-root)))
      ;; Create the file if it doesn't exist
      (unless (file-exists-p org-file)
        (with-temp-buffer
          (insert "#+TITLE: Project Tasks\n\n* Tasks\n")
          (write-file org-file)))
      ;; Ensure the "Tasks" heading exists
      (with-current-buffer (find-file-noselect org-file)
        (unless (org-find-exact-headline-in-buffer "Tasks")
          (goto-char (point-max))
          (insert "\n* Tasks\n")))
      org-file)))

(setq org-capture-templates
      '(("p" "Project Task" entry
         (file+headline my/project-org-file "Tasks")
         "* TODO %?\n  %U\n  %a")))
