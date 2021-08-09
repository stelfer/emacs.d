# emacs.d
This is my emacs configuration
On a new machine,

	$ cd ~
	$ git clone git@github.com:stelfer/emacs.d.git .emacs.d
	
Then launch emacs. If you want to adjust the font sizes, and maybe some other options, look in 

	~/.emacs.d/custom.el
	
At the end of `init.el`, there are various customizations that you can be disabled

```elisp
(require 'my-cc-mode)
(require 'my-lsp-mode)
(require 'my-text-mode)
(require 'my-org-mode)
(require 'my-epg-mode)
```

