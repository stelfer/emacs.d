
;; This here to reduce visual startup noise
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;;; System-specific
(cond
 ((eq system-type 'darwin)
  (progn
    (global-set-key [kp-delete] 'delete-char)
    (setq mac-option-modifier 	'alt
	  mac-command-modifier 	'meta)
    (setq initial-frame-alist '((left . 0) (top . 0) (width . 100) (height . 60))))

  )
 ((eq system-type 'windows-nt)
  (progn
    (add-to-list 'initial-frame-alist '(fullscreen . fullheight)))))

;;; Windowed
(add-hook 'window-setup-hook (lambda ()
			       (when (display-graphic-p)
				 (set-frame-font my/fixed-font t t)
				 (windmove-default-keybindings)
				 ;; (load-theme 'my-light)
				 (setq-default fill-columnn 	100)
				 (setq visible-bell 		nil
				       select-enable-clipboard 	t
				       select-enable-primary 	t))))

;; Basic configuration
(put 'upcase-region 'disabled nil)
(setq save-interprogram-paste-before-kill	t
      apropos-do-all 			  	t
      mouse-yank-at-point 	       		t
      require-final-newline 			t
      load-prefer-newer 			t
      ediff-window-setup-function 		'ediff-setup-windows-plain
      transient-mark-mode 			t
      linum-format 				"%4d "
      indent-tabs-mode 				nil
      comment-auto-fill-only-comments 		t
      font-lock-maximum-decoration 		t
      ad-redefinition-action 			'accept
      column-number-mode 			t
      max-mini-window-height 			1
      eshell-where-to-jump 			'begin
      eshell-review-quick-commands 		nil
      eshell-smart-space-goes-to-end 		t)

;;; Keep custom variables in their own file, so we can VC this
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)

  (cond
   ((eq system-type 'darwin)
    (progn
      (customize-save-variable 'my/serif-font "Georgia")
      (customize-save-variable 'my/sans-font "Verdana")
      (customize-save-variable 'my/fixed-font "Menlo")
      ))
   ((eq system-type 'gnu/linux)
    (progn
      (customize-save-variable 'my/serif-font "DejaVu Serif")
      (customize-save-variable 'my/sans-font "DejaVu Sans")
      (customize-save-variable 'my/fixed-font "DejaVu Sans Mono")
      ))
   ((eq system-type 'windows-nt)
    (progn
      (customize-save-variable 'my/serif-font "Georgia")
      (customize-save-variable 'my/sans-font "Verdana")
      (customize-save-variable 'my/fixed-font "Consolas")
      ))
   ))
(load custom-file)
	  
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
  (setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg")
  (make-directory package-gnupghome-dir t)
  (set-file-modes package-gnupghome-dir #o700)

  (package-initialize)

  (setq  package-check-signature nil)  
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update)
  (setq  package-check-signature 'allow-unsigned)
  (package-install 'use-package))

;;; From now on, use-package installs everything

(eval-when-compile 
  (require 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package all-the-icons
  :if window-system
  :ensure t
  :config
  ;; (all-the-icons-install-fonts)
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
  (use-package helm-config)
  (use-package helm-xref :ensure t)
  (use-package helm-projectile
    :ensure t
    ;; :bind (:map prog-mode-map ("C-<tab>" . helm-projectile-find-other-file))
    )
  
  (helm-mode 1)
  )

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)))

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


(define-prefix-command 'my-prog-mode-map)

(use-package magit
  :ensure t
  :bind (:map my-prog-mode-map ("g" . magit-status)))


(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;;; Finished with generic configuration add keymaps
(add-hook 'prog-mode-hook
	  (lambda()
	    (linum-mode t)
	    (local-set-key (kbd "C-c") 'my-prog-mode-map)))

;;; Any global key remaps
(global-set-key (kbd "M-%") 'query-replace-regexp)


;;; Load the optional configuration in lisp/
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'my-cc-mode)
(require 'my-lsp-mode)
(require 'my-text-mode)
(require 'my-org-mode)
(require 'my-epg-mode)
