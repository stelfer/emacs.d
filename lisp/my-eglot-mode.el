(use-package eglot
  :ensure t
  :bind (:map my-prog-mode-map ("n" . flymake-goto-next-error) ("p" . flymake-goto-previous-error))

    ;; :bind (:map my-prog-mode-map (("b" . helm-projectile-find-other-file)
    ;; 				  ("C-f" . helm-projectile-find-file)))

  
  :init
  (add-hook 'before-save-hook (lambda () (eglot-format-buffer)))
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  ;; (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  ;; (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  
  
  )

(add-hook 'c++-mode-hook #'eglot-ensure)

(provide 'my-eglot-mode)

