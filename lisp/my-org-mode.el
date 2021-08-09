
;;; From https://stackoverflow.com/a/27043756
(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(defun my/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun my/setup-agenda-buffer ()
  ;; For agenda files, we use different fonts
  (face-remap-add-relative 'default '(:family "Monospace"))    
  (face-remap-add-relative 'org-level-2 '(:inherit default))    
  (face-remap-add-relative 'org-level-1 '(:inherit default :weight bold))    
  (setq-default line-spacing nil)
  )

(use-package org
  :ensure t
  :hook ((org-after-todo-statistics . my/org-summary-todo))
  :bind ((:map global-map
	       ("C-c l" . #'org-store-link)
	       ("C-c a" . #'org-agenda)
	       ("C-c c" . #'org-capture)
	       ("C-c r" . #'my/org-archive-done-tasks)
	       ))
  
  :config
  (setq org-startup-with-inline-images t
	org-log-done t
	org-log-into-drawer t
	org-treat-insert-todo-heading-as-state-change t
	org-fontify-whole-heading-line t
	org-fontify-whole-block-delimiter-line t
	org-directory (concat my/user-home-directory "/shared/org")
	org-default-notes-file (concat org-directory "/notes.org")
	org-outline-path-complete-in-steps nil         
	org-refile-use-outline-path t
	org-hide-emphasis-markers t
	org-agenda-files `(,org-directory)
	org-refile-targets '((nil :maxlevel . 1) (org-agenda-files :maxlevel . 1)))

  (use-package helm-org
    :ensure t)
  
  (if (org-agenda-file-p (buffer-file-name))
      (my/setup-agenda-buffer)
    ;; 
    ;; Otherwise use default stuff
    ;; 
    (setq line-spacing 0.25)
    ;; (org-bullets-mode nil)

    (variable-pitch-mode t)

    ;; We display images in these contexts
    (setq org-startup-with-inline-images t)
    
    ;; Make some nice margins
    (progn
      (setq left-margin-width 2)
      (setq right-margin-width 2)
      (set-window-buffer nil (current-buffer))))
  
  ;; All buffers get visual line mode
  (visual-line-mode t)

    
  )


(provide 'my-org-mode)
