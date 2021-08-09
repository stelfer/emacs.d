;;; Various text and text-modes configuration

(add-hook 'text-mode-hook (lambda () 
			    (visual-line-mode t)
			    ;; (visual-fill-column-mode t)
			    (setq-default fill-column 80)
			    ;; (setq line-spacing 0.5)
			    ))


(use-package markdown-mode
  :ensure t
  :config
  (setq line-spacing 0.5)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (progn
    (setq left-margin-width 4)
    (setq right-margin-width 4)
    (set-window-buffer nil (current-buffer))))


(use-package ispell
  :config
  (setq ispell-program-name "hunspell"
	ispell-local-dictionary "en_US"
	ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,en_US-med") nil utf-8))
	ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)

  (use-package flyspell
    :ensure t
    :hook ((text-mode-hook . flyspell-mode))))




(provide 'my-text-mode)
