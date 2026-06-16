;;; modules/keybindings.el --- Global keybindings
;; Package-local bindings live in their respective module's use-package :bind.
;; This file collects standalone global bindings.

;; M-x ergonomics
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Kill/yank
(bind-key "C-w"     'backward-kill-word)
(bind-key "C-x C-k" 'kill-region)
(bind-key "C-c C-k" 'kill-region)

;; Window splitting (functions defined in editing.el)
(bind-key "C-x 3" 'my/hsplit-last-buffer)
(bind-key "C-x 2" 'my/vsplit-last-buffer)

;; Consult
(bind-key "C-x C-r" #'consult-recent-file)
(bind-key "C-x h"   #'consult-outline)
(bind-key "C-x b"   #'consult-buffer)
(bind-key "C-c h"   #'consult-history)

;; Ripgrep (consult-ripgrep replaces deadgrep at the same F5 slot)
(keymap-global-set "<f5>" #'consult-ripgrep)

;; Popper (package config in ui.el)
(keymap-global-set "C-`"   #'popper-toggle)
(keymap-global-set "M-`"   #'popper-cycle)
(keymap-global-set "C-M-`" #'popper-toggle-type)
