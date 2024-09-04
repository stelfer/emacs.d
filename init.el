
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






;;; For all prog modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; For the modes I use my keymap
(define-prefix-command 'my-prog-mode-map)

(dolist (prog-hook '(emacs-lisp-mode-hook
		     cpp-mode-hook))
  (add-hook prog-hook (lambda ()
			(local-set-key (kbd "C-c") 'my-prog-mode-map)))
  )


;;; straight.el config
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; From now on, use-package installs everything
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package nerd-icons
  :straight (nerd-icons
             :type git
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; (nerd-icons-install-fonts)
  
  )

(use-package doom-modeline
  :if window-system
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package recentf
  :ensure t
  ;; :bind (("C-x r" . recentf-open))
  :config
  (setq recentf-max-saved-items	200)
  (add-to-list 'recentf-exclude ".*\\.tmp.*")
  )

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("M-s l" . consult-line)
	 ;; ("C-y" . consult-yank-from-kill-ring)
	 )
  :init
  (recentf-mode)
  :config
  (setq consult-narrow-key "<") ;; "C-+"
  )

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))


(use-package rainbow-mode
  :ensure t
  :hook ((prog-mode . rainbow-mode)))

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))


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

(use-package tree-sitter-langs)

(require 'my-cc-mode)
(require 'my-eglot-mode)
(require 'my-text-mode)
(require 'my-org-mode)
(require 'my-epg-mode)
(require 'my-docker-mode)
(require 'my-elfeed-mode)
(require 'my-python-mode)
(require 'my-tex-mode)
(require 'my-lean-mode)


;; (setq auth-sources '("~/.authinfo.gpg"))

