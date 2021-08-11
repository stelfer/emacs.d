;; This here to reduce visual startup noise
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;;; Keep custom variables in their own file, so we can VC this
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  ;; Bootstrap custom file
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
      )))

  ;; If Hack is installed, use that before platform defaults
  (if (find-font (font-spec :name "Hack"))
      (customize-save-variable 'my/fixed-font "Hack"))
  
  (customize-save-variable 'my/window-width 100)
  (customize-save-variable 'my/window-height 60)
  (customize-save-variable 'my/default-directory "~/"))

(load custom-file)

;;;
;;; Automatically chose font size based on dpi, then normalize on a 100x60
;;; terminal This seems to give a good compromise of standardizing the display
;;; qualities and not relying on the fullheight portability issues across
;;; platforms and x11 server implementations
;;; 

(when (display-graphic-p)
  (defun my/dpi ()
    (let* ((attrs (car (display-monitor-attributes-list)))
           (size (assoc 'mm-size attrs))
           (sizex (cadr size))
           (res (cdr (assoc 'geometry attrs)))
           (resx (- (caddr res) (car res)))
           dpi)
      (catch 'exit
        ;; in terminal
        (unless sizex
          (throw 'exit 10))
        ;; on big screen
        (when (> sizex 1000)
          (throw 'exit 10))
        ;; DPI
        (* (/ (float resx) sizex) 25.4))))

  (defun my/preferred-font-size ()
    (let ( (dpi (my/dpi)) )
      (cond
       ((< dpi 110) 12)
       ((< dpi 130) 13)
       ((< dpi 160) 14)
       ((< dpi 180) 16)
       ((< dpi 200) 18)
       (t 14))))
  
  (set-frame-font (format "%s-%d" my/fixed-font (my/preferred-font-size)) t t)
  (set-frame-size (selected-frame) my/window-width my/window-height)

  )

;;; system-specific
(when (eq system-type 'darwin)
    (global-set-key [kp-delete] 'delete-char)
    (setq mac-option-modifier 	'alt
	  mac-command-modifier 	'meta))

;;; Windowed
(add-hook 'window-setup-hook
	  (lambda ()
            (windmove-default-keybindings)
            (setq visible-bell 		nil
                  select-enable-clipboard 	t
                  select-enable-primary 	t)))
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
		  eshell-smart-space-goes-to-end 	t)
	    (global-font-lock-mode t)))

;;; Put all global keys into our own map, then let global-map inherit
(define-prefix-command 'my-global-mode-map)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (set-keymap-parent global-map 'my-global-mode-map)
	    (define-key my-global-mode-map (kbd "M-%") 'query-replace-regexp)
	    (define-key my-global-mode-map (kbd "C-<tab>") 'switch-to-buffer)))

;;; We may add to this as we go
(define-prefix-command 'my-prog-mode-map)
(add-hook 'prog-mode-hook
	  (lambda()
	    (linum-mode t)
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
  (package-install 'use-package))
(package-initialize)

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
  ;; If all-the-icons aren't loading, then run
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
    :bind (:map my-prog-mode-map (("b" . helm-projectile-find-other-file)
				  ("C-f" . helm-projectile-find-file))))
  (helm-mode 1))

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

(use-package magit
  :ensure t
  :bind (:map my-prog-mode-map ("g" . magit-status)))

(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;;; Load the optional configuration in lisp/
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'my-cc-mode)
(require 'my-lsp-mode)
(require 'my-text-mode)
(require 'my-org-mode)
(require 'my-epg-mode)
(require 'my-docker-mode)
