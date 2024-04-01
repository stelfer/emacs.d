
(use-package elfeed
  :ensure t

  :config
  (setq elfeed-use-curl t
	elfeed-db-directory "~/shared/rss"
	elfeed-enclosure-default-dir "~/shared/rss"	
	)

  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
				:remove 'unread))


  ;; From https://github.com/skeeto/elfeed/issues/392#issuecomment-884857504
  (defun my/elfeed-db-remove-entry (id)
    "Removes the entry for ID"
    (avl-tree-delete elfeed-db-index id)
    (remhash id elfeed-db-entries))

  (defun my/elfeed-search-remove-selected ()
    "Remove selected entries from database"
    (interactive)
    (let* ((entries (elfeed-search-selected))
	   (count (length entries)))
      (when (y-or-n-p (format "Delete %d entires?" count))      
	(cl-loop for entry in entries
		 do (my/elfeed-db-remove-entry (elfeed-entry-id entry)))))
    (elfeed-search-update--force))



  )

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/shared/rss/elfeed.org"))

  )

;; (use-package elfeed-goodies
;;   :ensure t
;;   :config
;;   (elfeed-goodies/setup)
;;   )



(provide 'my-elfeed-mode)
