
;;; https://gist.github.com/habamax/290cda0e0cdc6118eb9a06121b9bc0d7

;;; Require tree-sitter and compile the grammer if it is missing
(cl-assert (treesit-available-p))
(condition-case nil
    (tree-sitter-require 'python)
  (error
   (treesit-install-language-grammar 'python)))

(use-package python-ts-mode
  :bind (:map my-prog-mode-map (
				("C-p" . run-python )
				("C-c" . python-shell-send-buffer)
				("C-o" . python-shell-switch-to-shell)
				(">"   . python-indent-shift-right)
				("<"   . python-indent-shift-left)
				))
  :init 
  (setq major-mode-remap-alist
	'((python-mode . python-ts-mode)))
  
  (add-hook 'python-ts-mode-hook #'eglot-ensure)

  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
	  )))

(provide 'my-python-mode)
