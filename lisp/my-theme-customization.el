(advice-add 'load-theme :after 
	    (lambda (theme &rest args)
	      (custom-theme-set-faces
	       theme
	       '(helm-ff-directory ((t (:extend t :foreground "cyan2" :weight bold))))
	       '(helm-ff-dotted-directory ((t (:foreground "DimGray"))))
	       '(helm-selection ((t (:extend t :inverse-video t))))
	       '(helm-source-header ((t (:inherit helm-header :weight bold :height 1.3)))))
	      (enable-theme theme)
	      ))

(provide 'my-theme-customization)
