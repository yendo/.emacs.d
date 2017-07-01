;; -*- Emacs-Lisp -*- 
;; 
;; pilrc-mode.el - 
;; 
;; Copyleft 2001 Lasse Munck Rasmussen (munck@control.auc.dk).
;; 
;; $Locker:  $
;;
;; Author          : Lasse Munck Rasmussen
;; Created On      : Tue Mar 20 18:01:00 2001
;; Last Modified By: Lasse Munck Rasmussen
;; Last Modified On: Thu Jun 14 18:32:32 2001
;; Update Count    : 378
;; 
;; HISTORY
;; 
;; INSTALL: put pilrc-mode.el in your load-path and insert these lines in your .emacs
;; (setq auto-mode-alist
;;       (cons '("\\.rcp$" . pilrc-mode) auto-mode-alist))
;; (setq interpreter-mode-alist
;;       (cons '("pilrc" . pilrc-mode)
;;             interpreter-mode-alist))
;; (autoload 'pilrc-mode "pilrc-mode" "PilRC editing mode." t)
;; 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax table almost completely stolen from py-mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar pilrc-mode-syntax-table nil
  "Syntax table used in `pilrc-mode' buffers.")
(if pilrc-mode-syntax-table nil
  (setq pilrc-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\} "){" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" pilrc-mode-syntax-table)
  ;; comment delimiters like c++
  (modify-syntax-entry ?\/ ". 124b" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\* ". 23" pilrc-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"  pilrc-mode-syntax-table) 
  )

(defvar pilrc-mode-abbrev-table nil
  "Abbrev table used while in pilrc mode.")
(define-abbrev-table 'pilrc-mode-abbrev-table nil)

(defvar pilrc-mode-map ()
  "Keymap used in `pilrc-mode' buffers.")
(if pilrc-mode-map
    nil              ; Do not change the keymap if it is already set up.
  (setq pilrc-mode-map (make-sparse-keymap))
  (define-key pilrc-mode-map "\t" 'pilrc-indent-line)
  )

(defun pilrc-mode ()
  "Major mode for editing PilRC scripts intended for humans 
 Special commands: \\{pilrc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'BeginsBefore)
  (make-local-variable 'PointTmp)
  (make-local-variable 'indentLevel)
  (make-local-variable 'emptyline)
  (use-local-map pilrc-mode-map)
  (set-syntax-table pilrc-mode-syntax-table)
  (setq major-mode 'pilrc-mode
	mode-name "PilRC"
	font-lock-defaults '(pilrc-font-lock-keywords)
	local-abbrev-table pilrc-mode-abbrev-table
	))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font lock stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar pilrc-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
			'("FORM" "MENU" "ALERT" "VERSION" "STRING" "STRINGTABLE" "CATEGORIES" "APPLICATIONICONNAME" "APPLICATION" "LAUNCHERCATEGORY" "ICON" "ICONFAMILY" "SMALLICON" "SMALLICONFAMILY" "BITMAPGREY" "BITMAPGREY16" "BITMAPCOLOR16" "BITMAPCOLOR" "BITMAPCOLOR16K" "BITMAPCOLOR24K" "BITMAPCOLOR32K" "BITMAPFAMILY" "BITMAPFAMILYSPECIAL" "TRAP" "FONT" "HEX" "DATA" "INTEGER" "TRANSLATION" "BITMAP")
			"\\|"))
	)
    (list
     ;; keywords
     (cons (concat "^\\(" kw1 "\\)\\b[ \n\t(]") 1)
     '("\\b\\(BEGIN\\|END\\)\\b"
       1 font-lock-type-face)
     ;; functions
     (list (concat "\\<\\(ID\\|MENUID\\|"
		   "FORM\\|HELPID\\)[ \t]+\\([A-Za-z_0-9]+\\)")
	   2 'font-lock-function-name-face)
     (list (concat "\\<\\(GROUP\\|BITMAP\\|"
		   "MENUID\\)[ \t]+\\([A-Za-z_0-9]+\\)") ;; ok but id in "bitmap id" gets coloured
	   2 'font-lock-variable-name-face)
     '("\\b\\(AUTOID\\|INCLUDE\\)\\b"
       1 font-lock-constant-face) ;; ok but doesn't get # in "#include"
     ))
  "Additional expressions to highlight in PilRC mode.")
(put 'pilrc-mode 'font-lock-defaults '(pilrc-font-lock-keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq BeginIndentLevel 4)
(setq PointTmp (point))
(setq indentLevel 0)
(setq emptyline 0)

(defun pilrc-set-indent-level ()
  "set indent level by counting begin's and end's"
  (interactive)
  (setq BeginsBefore 0)
  (setq EndsBefore 0)
  (save-excursion ;save point position
    (point-to-register PointTmp)
    (end-of-line)
    (while (search-backward-regexp "\\b\\(begin\\|BEGIN\\)\\b" (point-min) t)
      (setq BeginsBefore (+ 1 BeginsBefore))
      )
    (if (> BeginsBefore 0) ; if begin before
	(progn
	  (register-to-point PointTmp)
	  (end-of-line)
	  (while (search-backward-regexp "\\b\\(end\\|END\\)\\b" (point-min) t)
	    (setq EndsBefore (+ 1 EndsBefore))
	    )
	  (setq  BeginIndentLevel (* 4 (- BeginsBefore EndsBefore)))
	  (if (> BeginsBefore EndsBefore)
	      (setq indentLevel (+ BeginIndentLevel 4)))
	  (if (eq BeginsBefore EndsBefore)
	      (setq indentLevel BeginIndentLevel)))
      (setq indentLevel 0)
      ) 
    )
  )

(defun pilrc-indent-line ()
  "Indent a line properly according to PilRC file format."
  (interactive)
  (pilrc-set-indent-level)
  (save-excursion
    (beginning-of-line)
    (if (eq (point) (point-at-eol)) ; if empty line
	(progn
	  (indent-line-to indentLevel)
	  (setq emptyline 1))
      ; else
      (setq emptyline 0)
      (beginning-of-line) ; set pointer at beginning of line
      (if (search-forward-regexp "\\b\\(BEGIN\\)\\b" (point-at-eol) t) ; if begin
	  (progn
	    (indent-line-to BeginIndentLevel)
	    )
	(beginning-of-line)
	(if (search-forward-regexp "\\b\\(END\\)\\b" (point-at-eol) t) ; if end
	    (progn
	      (indent-line-to (+ BeginIndentLevel 4))
	      )
	  (indent-line-to indentLevel)))
      (beginning-of-line)
      )
    )
  (if (eq emptyline 1)
      (end-of-line))
  )
