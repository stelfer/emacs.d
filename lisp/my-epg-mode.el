;; We enable this in order to use epa on the fly
;; Particularly for the password file
;; 
;;
;; On windows and linux, for pinentry, we need to follow
;; https://emacs.stackexchange.com/a/32882
;; 
;; Put this in your ~/.gnupg/gpg-agent.conf:
;;
;; allow-emacs-pinentry
;; allow-loopback-pinentry
;;
;; Then tell gpg-agent to load this configuration with gpgconf in a shell:
;;
;; $ gpgconf --reload gpg-agent

;;;
;;; From various kinds of advice from stackoverflow, break class in case we can't use pinentry
;;; 
(defun my/epg-fix-elpa-keyring ()
  (interactive)
  (setq  package-check-signature nil)
  (package-reinstall 'gnu-elpa-keyring-update)
  (setq  package-check-signature 'allow-unsigned))

(use-package pinentry
  :ensure t)

(use-package epg
  :after (pinentry)
  :config
  (let (gpg-path)
    (cond
     ((eq system-type 'darwin)
      (progn
	(setq gpg-path "/usr/local/MacGPG2/bin")
	(setq epg-gpg-program "gpg2")
	))
     ((eq system-type 'gnu/linux)
      (progn
	;; FILL IN HERE
	)
      )
     ((eq system-type 'windows-nt)
      (progn
	;; For whatever reason, the default path doesn't work
	(setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg")
	)))
    (setenv "PATH" (concat (getenv "PATH") (concat ":" gpg-path)))
    (add-to-list 'exec-path gpg-path))

  ;; per SO referenced above
  (setq epg-pinentry-mode 'loopback))


(provide 'my-epg-mode)
