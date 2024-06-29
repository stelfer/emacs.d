
(use-package flycheck
  :ensure t)
(use-package lsp-mode
  :ensure t)

(add-to-list 'load-path (expand-file-name "lisp/lean4-mode" user-emacs-directory))

(require 'lean4-mode)

(provide 'my-lean-mode)

