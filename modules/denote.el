;;; modules/denote.el --- Denote: lightweight, DB-free Zettelkasten

;; Denote stores everything in the filesystem via a naming convention
;; (IDENTIFIER--title__keywords.org). No database, nothing to corrupt or
;; rebuild. Search/links are powered by the existing completion stack
;; (vertico/consult/orderless/embark) plus ripgrep.

;; --- Where the notes live -----------------------------------------------------
;; Change this one line to relocate the vault. The migrated notes currently
;; sit in the brain_two repo; move them somewhere permanent and update here.

(defvar my/denote-dir (expand-file-name "~/notes")
  "Root directory of the Denote vault.")

;; --- Denote core --------------------------------------------------------------

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)                 ; new note
         ("C-c n N" . denote-type)            ; new note, choose file type
         ("C-c n o" . denote-open-or-create)  ; jump to / create by title
         ("C-c n i" . denote-link)            ; insert a link
         ("C-c n I" . denote-link-or-create)  ; insert link, create if missing
         ("C-c n b" . denote-backlinks)       ; show what links here (grep-based)
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)
         ("C-c n k" . denote-rename-file-keywords)
         :map dired-mode-map
         ("C-c n r" . denote-dired-rename-marked-files))
  :custom
  (denote-directory my/denote-dir)
  (denote-file-type nil)                      ; nil = Org
  (denote-prompts '(title keywords))
  (denote-sort-keywords t)
  (denote-infer-keywords t)
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  ;; Seed keyword completion with the taxonomy from the migrated vault.
  (denote-known-keywords
   '("work" "person" "meeting" "oneonone" "journal" "source" "index" "inbox"
     "digistore24" "acquisition" "activation" "engagement" "platform" "ai"
     "hiring" "onboarding" "process" "agile" "analytics" "payments"))
  :config
  (denote-rename-buffer-mode 1))              ; buffer names reflect note titles

;; --- Org-specific Denote features (backlink dblocks, heading links) -----------

(use-package denote-org
  :after (denote org)
  :bind (("C-c n h" . denote-org-link-to-heading)
         ("C-c n d" . denote-org-dblock-insert-links)
         ("C-c n D" . denote-org-dblock-insert-backlinks)))

;; --- Daily journal ------------------------------------------------------------
;; Flat layout: journal entries live in the vault root, tagged :journal:.

(use-package denote-journal
  :after denote
  :bind (("C-c n j" . denote-journal-new-or-existing-entry))
  :custom
  (denote-journal-directory nil)              ; nil = use denote-directory (flat)
  (denote-journal-keyword "journal")
  (denote-journal-title-format 'day-date-month-year))

;; --- Folgezettel / sequenced notes (optional Zettelkasten numbering) ----------

(use-package denote-sequence
  :after denote
  :bind (("C-c n s" . denote-sequence)
         ("C-c n S" . denote-sequence-dired)))

;; --- consult integration: live search across notes ----------------------------

(use-package consult-denote
  :after (denote consult)
  :bind (("C-c n f" . consult-denote-find)    ; find note by name
         ("C-c n g" . consult-denote-grep))   ; full-text search the vault
  :config
  (consult-denote-mode 1))

;; --- Exploration: random note, duplicates, broken links, keyword charts -------

(use-package denote-explore
  :after denote
  :bind (("C-c n x r" . denote-explore-random-note)
         ("C-c n x d" . denote-explore-identify-duplicate-notes)
         ("C-c n x z" . denote-explore-zero-keywords)
         ("C-c n x k" . denote-explore-barchart-keywords)
         ("C-c n x b" . denote-explore-barchart-filetypes)))

;; --- Note templates -----------------------------------------------------------
;; Body scaffolds for the note types carried over from the Obsidian vault.
;; Use them via `C-u C-c n n` (which adds a template prompt) or the wrapper
;; commands below.

(with-eval-after-load 'denote
  (setq denote-templates
        '((person . "* Details\n- Organization: \n- Role: \n- Birthday: \n- Email: \n- Location: \n\n* Notes\n\n* References\n")
          (meeting . "* Attendees\n\n* Notes\n\n* Action Items\n- [ ] \n\n* References\n")
          (oneonone . "* Mood\n\n* Topics\n\n* Action Items\n- [ ] \n")
          (daily . "* Health\n* Mood\n* TODO\n- [ ] \n* Notes\n")
          (note . "\n* References\n"))))

(defun my/denote-new (keywords template)
  "Create a Denote note with fixed KEYWORDS and TEMPLATE, prompting for title."
  (denote (denote-title-prompt) keywords 'org nil nil template))

(defun my/denote-person ()
  "New person note."
  (interactive) (my/denote-new '("person") 'person))

(defun my/denote-meeting ()
  "New meeting note."
  (interactive) (my/denote-new '("work" "meeting") 'meeting))

(defun my/denote-one-on-one ()
  "New one-on-one note."
  (interactive) (my/denote-new '("work" "oneonone") 'oneonone))

;; Bound unconditionally; the wrapper commands autoload Denote when invoked.
(bind-key "C-c n t p" #'my/denote-person)
(bind-key "C-c n t m" #'my/denote-meeting)
(bind-key "C-c n t 1" #'my/denote-one-on-one)

(provide 'denote-config)
;;; denote.el ends here
