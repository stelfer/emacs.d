
;;; system-specific
(when (eq system-type 'darwin)
    (global-set-key [kp-delete] 'delete-char)
    (setq mac-option-modifier 	'alt
	  mac-command-modifier 	'meta))

(when (or (eq system-type 'darwin)
	  (eq system-type 'gnu/linux)
	  (eq system-type 'cygwin))
  (setenv "PATH" (concat (getenv "PATH") (concat ":" "/usr/local/bin")))
  (add-to-list 'exec-path "/usr/local/bin"))

;;; Windowed
(add-hook 'window-setup-hook
	  (lambda ()
	    (load-theme my/theme t)
            (windmove-default-keybindings)
            (setq visible-bell 		nil
                  select-enable-clipboard 	t
                  select-enable-primary 	t)

	    ;; Wayland clipboard workaround
	    (when (and (getenv "WAYLAND_DISPLAY") (not (equal (getenv "GDK_BACKEND") "x11")))
	      (setq
	       interprogram-cut-function
	       (lambda (text)
		 "from https://github.com/microsoft/wslg/issues/15#issuecomment-1447561734"
		 (start-process "wl-copy" nil "wl-copy" "--trim-newline" "--type" "text/plain;charset=utf-8"  text))))
	    )
	  )
;;; Emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq-default fill-column 80)
	    (setq default-directory			my/default-directory
		  save-interprogram-paste-before-kill	t
		  apropos-do-all 			t
		  mouse-yank-at-point 	       		t
		  require-final-newline 		t
		  load-prefer-newer 			t
		  ediff-window-setup-function 		'ediff-setup-windows-plain
		  transient-mark-mode 			t
		  linum-format 				"%4d "
		  indent-tabs-mode 			nil
		  comment-auto-fill-only-comments 	t
		  font-lock-maximum-decoration 		t
		  ad-redefinition-action 		'accept
		  column-number-mode 			t
		  max-mini-window-height 		1
		  eshell-where-to-jump 			'begin
		  eshell-review-quick-commands 		nil
		  eshell-smart-space-goes-to-end 	t
		  scroll-preserve-screen-position 	1)
	    (global-font-lock-mode t)))

;;; Put all global keys into our own map, then let global-map inherit
(define-prefix-command 'my-global-mode-map)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (set-keymap-parent global-map 'my-global-mode-map)
	    (define-key my-global-mode-map (kbd "M-%") 'query-replace-regexp)
	    (define-key my-global-mode-map (kbd "C-<tab>") 'switch-to-buffer)
	    (define-key my-global-mode-map (kbd "M-n") (kbd "C-u 1 C-v"))
	    (define-key my-global-mode-map (kbd "M-p") (kbd "C-u 1 M-v"))))

;;; We may add to this as we go
(define-prefix-command 'my-prog-mode-map)
(add-hook 'prog-mode-hook
	  (lambda()
	    (display-line-numbers-mode)
	    (local-set-key (kbd "C-c") 'my-prog-mode-map)))


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(unless (file-directory-p package-user-dir)
  ;; 
  ;; Hard to believe this is as broken as it is.  In order to install
  ;; some packages later, like pinentry, we need to have the updated
  ;; gnu public key. That is available via the package
  ;; `gnu-elpa-keyring-update`. However, this package won't be visible
  ;; here unless we can validate the signature for the gnu
  ;; archives. This means that we need to use the gpg infrastructure
  ;; if it's installed. For some reason, `package-gnupghome-dir`
  ;; doesn't follow paths absolutely. This means that we need to set
  ;; the variable explicitly here. We also need to fix permissions for
  ;; various versions of gpg
  (setq package-gnupghome-dir (expand-file-name "elpa/gnupg" user-emacs-directory))
  (make-directory package-gnupghome-dir t)
  (set-file-modes package-gnupghome-dir #o700)

  (package-initialize)

  ;; If we try to refresh melpa too at this point, it seems to overrun
  ;; the buffers, and untar of `gnu-elpa-keyring-update` transiently
  ;; fails
  (let ((package-archives '(("gnu" . "https://elpa.gnu.org/packages/"))))
    (setq  package-check-signature nil)  
    (package-refresh-contents nil)
    (package-install 'gnu-elpa-keyring-update)
    (setq  package-check-signature 'allow-unsigned))

  (package-refresh-contents)
  ;; (package-install 'use-package)
  )
(package-initialize)

;;; From now on, use-package installs everything

(eval-when-compile 
  (require 'shortdoc)			  ;; This is missing for some reason???
  (require 'use-package)
  )

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-show-preview t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions nil)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-show-preview t)  
  (auto-package-update-maybe))

(use-package nerd-icons
  :if (display-graphic-p)
  :ensure t
  :config
  ;; (nerd-icons-install-fonts)
  )

(use-package doom-modeline
  :if window-system
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package helm
  :ensure t
  :bind ((:map helm-map ("<tab>" . helm-execute-persistent-action))
	 (:map global-map
	       ([remap execute-extended-command] . #'helm-M-x)
	       ([remap find-file] . #'helm-find-files)
	       ([remap switch-to-buffer] . #'helm-mini)))
  :config
  (setq helm-split-window-in-side-p           nil
	helm-move-to-line-cycle-in-source     t
	helm-ff-search-library-in-sexp        t
	helm-scroll-amount                    8
	helm-ff-file-name-history-use-recentf t)
;;  (use-package helm-config :ensure t)
  (use-package helm-xref :ensure t)
  (use-package helm-projectile
    :ensure t
    :bind (:map my-prog-mode-map (("b" . helm-projectile-find-other-file)
				  ("C-f" . helm-projectile-find-file)
				  ("r" . eglot-rename))))
  (helm-mode 1))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode 1))

;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))

(use-package rainbow-mode
  :ensure t
  :hook ((prog-mode . rainbow-mode)))

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items	200)
  (add-to-list 'recentf-exclude ".*\\.tmp.*"))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode       . enable-paredit-mode)
	 (eval-expression-minibuffer-setup . enable-paredit-mode)
	 (ielm-mode             . enable-paredit-mode)
	 (lisp-mode             . enable-paredit-mode)
	 (lisp-interaction-mode . enable-paredit-mode)
	 (scheme-mode           . enable-paredit-mode)))

(use-package speed-type
  :ensure t)

(use-package magit
  :ensure t
  :bind (:map my-prog-mode-map ("g" . magit-status))
  )

(use-package forge
  :ensure t
  :after magit)

(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq
    magit-delta-default-dark-theme "Monokai Extended"
    magit-delta-default-light-theme "Monokai Extended Light"
    magit-delta-hide-plus-minus-markers nil)
  )

(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;;; Load the optional configuration in lisp/
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'my-cc-mode)
(require 'my-eglot-mode)
(require 'my-text-mode)
(require 'my-org-mode)
(require 'my-epg-mode)
(require 'my-docker-mode)
(require 'my-elfeed-mode)

(setq auth-sources '("~/.authinfo.gpg"))

