
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp-deferred)
	 (c-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
	  read-process-output-max (* 1024 1024)
	  treemacs-space-between-root-nodes nil
	  company-idle-delay 0.0
	  company-minimum-prefix-length 1
	  lsp-idle-delay 0.1)  ;; clangd is fast

  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  (use-package dap-mode)
  (use-package dap-cpptools)

  (yas-global-mode)

  (lsp-enable-which-key-integration)
  
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd-12")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  )

(provide 'my-lsp-mode)
