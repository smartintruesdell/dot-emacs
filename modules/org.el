;;; modules/org.el --- Org-mode configuration

(require 'org)
(require 'project)

(defun my/project-org-file ()
  "Return the path to the project's tasks.org, creating it if necessary."
  (let* ((project (or (project-current t)
                      (error "Not in a project")))
         (project-root (project-root project)))
    (let ((org-file (expand-file-name "tasks.org" project-root)))
      (unless (file-exists-p org-file)
        (with-temp-buffer
          (insert "#+TITLE: Project Tasks\n\n* Tasks\n")
          (write-file org-file)))
      (with-current-buffer (find-file-noselect org-file)
        (unless (org-find-exact-headline-in-buffer "Tasks")
          (goto-char (point-max))
          (insert "\n* Tasks\n")))
      org-file)))

(setq org-capture-templates
      '(("p" "Project Task" entry
         (file+headline my/project-org-file "Tasks")
         "* TODO %?\n  %U\n  %a")))
