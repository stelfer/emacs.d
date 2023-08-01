(use-package cmake-mode
  :ensure t)

(use-package modern-cpp-font-lock
  :ensure t)

(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)


;;; In order to generate a emacs cc style, we use the trick here
;;; https://stackoverflow.com/a/39907217
;;; cd to some big cc project
;;;
;;; $ find src include -name \*.cpp -or -name \*.h -exec cat {} >> bigfile.cpp \;
;;; $ emacs bigfile.cpp
;;;
;;;  Then M-x c-guess-no-install and M-x c-guess-view

;;; So for :
;; ---
;; BasedOnStyle: Google
;; AccessModifierOffset: '-2'
;; AlwaysBreakAfterReturnType: TopLevel
;; AlwaysBreakTemplateDeclarations: 'Yes'
;; BinPackArguments: 'false'
;; BinPackParameters: 'false'
;; BreakBeforeBraces: Linux
;; Standard: Cpp11
;; TabWidth: '8'
;; UseTab: Never
;; IndentWidth: 4
;; AlignConsecutiveAssignments: 'true'
;; ...
;;
;; We have

(c-add-style "my-cc-style"
	     '("gnu"
	       (c-basic-offset . 4)
	       (c-offsets-alist
		(access-label . *)
		(arglist-cont . 0)
		(arglist-intro . +)
		(block-close . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro . +)
		(brace-list-open . 0)
		(case-label . +)
		(class-close . 0)
		(class-open . 0)
		(cpp-define-intro . +)
		(cpp-macro-cont . +)
		(defun-block-intro . +)
		(defun-close . 0)
		(defun-open . 0)
		(else-clause . 0)
		(func-decl-cont . 0)
		(inclass . +)
		(inline-close . 0)
		(innamespace . 0)
		(label . 0)
		(member-init-cont . 0)
		(member-init-intro . +)
		(namespace-close . 0)
		(namespace-open . 0)
		(statement . 0)
		(statement-block-intro . +)
		(statement-case-intro . +)
		(statement-cont . +)
		(stream-op . 7)
		(substatement . +)
		(template-args-cont . +)
		(topmost-intro . 0)
		(topmost-intro-cont . 0)
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont-nonempty . c-lineup-arglist)
		(block-open . 0)
		(brace-entry-open . 0)
		(c . c-lineup-C-comments)
		(catch-clause . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-macro . -1000)
		(do-while-closure . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . +)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . c-lineup-inexpr-block)
		(inline-open . 0)
		(inmodule . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . 5)
		(lambda-intro-cont . +)
		(module-close . 0)
		(module-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-open . +)
		(string . -1000)
		(substatement-label . 0)
		(substatement-open . +))))


(add-hook 'c++-mode-hook (lambda ()
			 (c-set-style "my-cc-style")))


(provide 'my-cc-mode)
