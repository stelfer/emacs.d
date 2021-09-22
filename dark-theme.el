(deftheme dark  "Dark theme")

(defface my/string-face '((t (:inherit default :extend t :foreground "#e5c69c")))
  "A nice string color"
  )

(defvar-local *bg* "#242424")
(defvar-local *fg* "#f6f3e8")
(defvar-local *context* "#e5c69c")
(defvar-local *context-dark* "#f0c674")

(defvar-local *bg-contrast* "#444444")
(defvar-local *fg-bright* "#e1f4dc")

(defvar-local *black*   *bg*)
(defvar-local *red*     "#e5786d")
(defvar-local *green*   "#8af29b")
(defvar-local *yellow*  "#e0f28a")
(defvar-local *blue*    "#8ac6f2")
(defvar-local *magenta* "#f28ac6")
(defvar-local *cyan*    "#8af2e9")
(defvar-local *white*   *fg*)

(defvar-local *purple* "#b58af2")
(defvar-local *orange* "#f0c674")
(defvar-local *aqua*   "#8af2cf")

(defvar-local *inactive* "#99968b")

;;; Pallette 
;; "#f2b58a"
;; "#ddaa6f"
;; "#cae682"
;; "#e5c69c"
;; "#5f99cc"
;; "#f0c674"
;; "#cc9494"
;; "#e5c69c"
;; "#66cccc"
;; "#f6f3e8"
;; "#95e454"
;; "#cae682"
;; "#99968b"
;; "#f0f4e6"
;; "#39f4e6"

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'dark
   `(default ((,class (:inherit nil :extend nil :foreground ,*fg* :background ,*bg*))))   

   `(cursor ((,class (:background ,*context*))))
   `(fringe ((,class (:background "#303030"))))

   `(highlight ((,class (:background ,*bg-contrast* :foreground ,*fg-bright* :underline t))))

   `(region ((,class (:background ,*bg-contrast* :foreground ,*fg-bright*))))
   `(secondary-selection ((,class (:background "#333366" :foreground ,*fg-bright*))))
   `(isearch ((,class (:background "#343434" :foreground "#857b6f"))))
   `(lazy-highlight ((,class (:background "#384048" :foreground "#a0a8b0"))))

   `(header-line ((,class (:foreground ,*context*))))
   `(minibuffer-prompt ((,class (:foreground ,*context*))))
   `(escape-glyph ((,class (:foreground ,*context-dark* :weight bold))))
   `(homoglyph ((,class (:foreground ,*context-dark* :weight bold))))
   
   `(mode-line ((,class (:background ,*bg-contrast* :foreground ,*fg-bright*))))
   `(mode-line-inactive ((,class (:background ,*bg-contrast* :foreground ,*inactive*))))

   `(doom-modeline-project-dir ((,class (:inherit mode-line :weight bold))))
   `(doom-modeline-buffer-path ((,class (:inherit mode-line))))
   `(doom-modeline-buffer-file ((,class (:inherit mode-line :slant italic))))
   
   `(font-lock-comment-face ((,class (:extend t :foreground ,*inactive*))))
   `(font-lock-function-name-face ((,class (:foreground ,*blue*))))
   `(font-lock-string-face ((,class (:extend t :foreground ,*context*))))
   `(font-lock-keyword-face ((,class (:foreground ,*cyan*))))
   `(font-lock-constant-face ((,class (:foreground ,*orange*))))
   `(font-lock-type-face ((,class (:foreground ,*fg-bright*))))


   `(helm-ff-directory ((,class (:inherit dired-directory))))
   `(helm-selection ((,class (:extend t :background ,*bg-contrast* :weight bold))))
   `(helm-source-header ((,class (:inherit helm-header :weight bold :height 1.0))))
   `(helm-ff-file-extension ((,class (:foreground ,*aqua*))))

   `(magit-diff-added ((,class (:foreground ,*green*))))
   `(magit-diff-added-highlight ((,class (:foreground ,*fg-bright*))))
   `(magit-diff-removed ((,class (:foreground ,*red*))))
   `(magit-diff-removed-highlight ((,class (:foreground ,*fg-bright*))))
   `(git-commit-summary ((,class (:foreground ,*fg*))))

   )
)


(custom-theme-set-variables
 'dark
 `(ansi-color-names-vector [,*black*,*red*,*green*,*yellow*,*blue*,*magenta*,*cyan*,*white*]))


(provide-theme 'dark)
