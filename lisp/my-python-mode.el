; For eglot, we need an LSP provider
; on ubuntu
; $ sudo apt install python3-pylsp

(require 'my-tree-sitter)
(tree-sitter-require 'python)
(add-hook 'python-mode-hook #'tree-sitter-mode)

(use-package python-mode
  :bind (:map my-prog-mode-map (
				("C-p" . run-python )
				("C-c" . python-shell-send-buffer)
				("C-o" . python-shell-switch-to-shell)
				(">"   . python-indent-shift-right)
				("<"   . python-indent-shift-left)
				))
  :config 
  
  (add-hook 'python-mode-hook #'eglot-ensure)

  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
	  )))

(provide 'my-python-mode)
