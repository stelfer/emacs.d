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
  
  (customize-save-variable 'my/default-directory "~/")
  (customize-save-variable 'my/theme 'dark)

  ;; Our preferred font size is 3*dots-per-mm - 1
  (defun my/vert-dpmm ()
    (let* ((attrs (car (display-monitor-attributes-list)))
           (size (assoc 'mm-size attrs))
           (sizey (caddr size))
           (res (cdr (assoc 'geometry attrs)))
           (resy (- (cadddr res) (cadr res))))
      `(,(floor (/ sizey 13.5)))))
  (defun my/preferred-font-size () (car (my/vert-dpmm)))
  (customize-save-variable 'my/font-size (my/preferred-font-size))

  ;; Use the golden ratio... cause why not
  (defun my/preferred-window-height (width) (ceiling (/ width 1.618033988)))
  (setq my/default-window-width 110)
  
  (customize-save-variable 'my/window-width my/default-window-width)
  (customize-save-variable 'my/window-height
			   (my/preferred-window-height my/default-window-width)))

(load custom-file)

(setq default-frame-alist
      `((height . ,my/window-height)
        (width . ,my/window-width)
        (left . 0)
        (top . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)
	(menu-bar-lines . 0)
	(font . ,(format "%s:pixelsize=%d" my/fixed-font my/font-size))
	))

(load-theme my/theme t)

