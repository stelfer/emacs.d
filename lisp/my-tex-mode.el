;;; Various text and text-modes configuration


(use-package pdf-tools
  :ensure t)

(use-package auctex
  :ensure t

  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)  
  )

(use-package auctex-latexmk
  :ensure t

  :init
  (auctex-latexmk-setup)  
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)

  )



(provide 'my-tex-mode)
