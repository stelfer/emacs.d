
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp)
	 (c-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :bind (:map my-prog-mode-map (("<tab>" . lsp-format-region)))
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
	  read-process-output-max (* 1024 1024)
	  treemacs-space-between-root-nodes nil
	  company-idle-delay 0.0
	  company-minimum-prefix-length 1
	  lsp-idle-delay 0.1)  ;; clangd is fast

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)
  (use-package helm-lsp
    :ensure t
    :commands helm-lsp-workspace-symbol)
  (use-package dap-mode :ensure t)
  ;; (use-package dap-cpptools :ensure t)
  (use-package dap-cpptools)

  (use-package flycheck :ensure t)
  
  (use-package yasnippet
    :ensure t
    :config
    (yas-global-mode))
  

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "clangd-12")
  ;;                   :major-modes '(c-mode c++-mode)
  ;;                   :remote? t
  ;;                   :server-id 'clangd-remote))
  )

(provide 'my-lsp-mode)
