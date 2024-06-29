
(use-package flycheck
  :ensure t)
(use-package lsp-mode
  :ensure t)

(add-to-list 'load-path (expand-file-name "lisp/lean4-mode" user-emacs-directory))


(use-package lean4-mode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  ;; to defer loading the package until required
  :commands (lean4-mode))

;; (require 'lean4-mode)

(provide 'my-lean-mode)

