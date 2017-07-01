(defun procmail-mode ()
  "Mode for highlighting procmailrc files"
  (interactive)
  (setq mode-name "Procmail"
	major-mode 'procmail)

  (require 'font-lock)
  (make-local-variable 'font-lock-defaults)
  (setq procmail-font-lock-keywords
	(list '("#.*"
		. font-lock-comment-face)
	      '("Return-Path:"
		. font-lock-type-face) ; green
	      '("^[\t ]*:.*"
		. font-lock-function-name-face) ; blue ?
	      '("[A-Z_]+=.*"
					; '("[A-Za-z_]+=.*"
		. font-lock-keyword-face)
	      '("^[\t ]*\\*.*"
		. font-lock-doc-face) ; light brown
					; '("\$[A-Za-z0-9_]+"
	      '("^[\t ]*ml\/"
		. font-lock-builtin-face) ; violet
	      '("^[\t ]*from\/"
		. font-lock-reference-face)
	      '("^[\t ]*junk\/"
		. font-lock-constant-face) ; turquoize
	      '("^[\t ]*dm/"
		. font-lock-warning-face) ; red
	      '("^[\t ]*not-found/"
		. font-lock-variable-name-face) ; orange
	      '("^[\t ]*to/"
		. font-lock-string-face) ; light brown
	      ))
  (setq font-lock-defaults '(procmail-font-lock-keywords t))
  (font-lock-mode t) )
