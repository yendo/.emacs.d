;;; pmemo.el --- palm memo

;;; Commentary:
;; 


;;; History:
;; 
;; 2003-01-06 initial coding.

;;; Code:


(setq pmemo-split-window-size 0.4)

(defvar pmemo-command "~/bin/palm-memo.pl")
(defvar pmemo-split-window-size 0.4)

;; mode

(defun pmemo-mode ()
  "Major mode for viewing palm memo database.

f -- Show the memo at point.
n -- Show next memo.
p -- Show previous memo.
s -- Search memo.
r -- Redisplay all.
o -- Other window.
q -- Quit."
  (kill-all-local-variables)

  (setq buffer-read-only t)

  (setq major-mode 'pmemo-mode
	mode-name "Palm Memo List")
  (setq pmemo-mode-map (make-keymap))
  (suppress-keymap pmemo-mode-map t)
  (define-key pmemo-mode-map "q" 'pmemo-quit)
  (define-key pmemo-mode-map "r" 'pmemo)
  (define-key pmemo-mode-map "n" 'pmemo-next)
  (define-key pmemo-mode-map "p" 'pmemo-previous)
  (define-key pmemo-mode-map "s" 'pmemo-search)
  (define-key pmemo-mode-map [return] 'pmemo-o)
  (define-key pmemo-mode-map "f" 'pmemo-o)
  (define-key pmemo-mode-map " " 'pmemo-o)
  (define-key pmemo-mode-map "o" 'other-window)


  (use-local-map pmemo-mode-map)

  (setq header-line-format
	(format "[%s] %d ·ï"
		pmemo-category (pmemo-count-lines-page)))

  (message "Commands: f, n, p, s, r; q to quit.")
  (run-hooks 'pmemo-mode-hook)
  )


(defun pmemo-contens-mode ()
  "Major mode for viewing palm memo contents.

o -- Other window.
q -- Close memo contents buffer."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'pmemo-contens-mode
	mode-name "Palm Memo Contents")
  (setq pmemo-mode-map (make-keymap))
  (suppress-keymap pmemo-mode-map t)

  (define-key pmemo-mode-map "q" 'pmemo-contens-close)
  (define-key pmemo-mode-map "o" 'other-window)

  (use-local-map pmemo-mode-map)
  (run-hooks 'pmemo-contens-mode-hook)
  )



(defun pmemo-quit ()
  "Quit palm memo mode."
  (interactive)
  (delete-other-windows)
  (kill-buffer "*Palm Memo List*")
  (kill-buffer (get-buffer "*Palm Memo*"))
)


(defun pmemo-contens-close ()
  "Quit palm memo mode."
  (interactive)
  (delete-window)
  (kill-buffer (get-buffer "*Palm Memo*"))
)



;; Headers

(defun pmemo ()
  "Display memo headers."
  (interactive)
  (pmemo-list nil))

(defun pmemo-search ()
  "Search memo."
  (interactive)
  (pmemo-list (read-from-minibuffer "Search: ")))


(defun pmemo-list (str)
  "Show memo list.  Argument STR search string."
  (let ((resize-mini-windows nil))
    (save-excursion
    (delete-other-windows)
    (message "Displaying memo headers...")
    (switch-to-buffer (get-buffer-create "*Palm Memo List*"))
    (setq buffer-read-only nil)
    (setq standard-output (current-buffer))
    (erase-buffer)
    (shell-command
     (concat pmemo-command (when str (concat " -h " "\"" str "\"")))
     (current-buffer))
    (setq pmemo-category (if str "¸¡º÷" "¤¹¤Ù¤Æ"))
  
    (pmemo-mode)
    )))


;; Contents
	
(defun pmemo-next ()
  "Show next memo."
  (interactive)
  (forward-line)
  (pmemo-o)
  )

(defun pmemo-previous ()
  "Show previous memo."
  (interactive)
  (forward-line -1)
  (pmemo-o)
  )


(defun pmemo-o ()
  "Open a memo in another window."
  (interactive)
  (save-excursion
    (setq num (pmemo-find-num))
    (if (one-window-p)
	(split-window-vertically
	 (round (* (frame-height) pmemo-split-window-size))))
	 
    (select-window (next-window))
    
    (pmemo-visit-memo num)
    (select-window (next-window))
    ))



(defun pmemo-f ()
  "Open a memo in this window."
  (interactive)
  (save-excursion
    (pmemo-visit-memo (pmemo-find-num))
    ))


(defun pmemo-find-num ()
  "Get memo number from memo list buffer."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^ *\\([0-9]*\\) " nil nil)
    (buffer-substring (match-beginning 1) (match-end 1))
    ))


(defun pmemo-visit-memo (num)
  "Display a memo of Palm MemoDB.  Argument NUM memo number."
  (let ((resize-mini-windows nil))
  (save-excursion
    (switch-to-buffer (get-buffer-create "*Palm Memo*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (shell-command (concat pmemo-command " " num) "*Palm Memo*")
    (goto-line 1)
    (pmemo-contens-mode)
    )))



; textmodes/page

(defun pmemo-count-lines-page ()
  "Report number of lines on current page."
  (interactive)
  (save-excursion
    (let (beg end)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))

      (count-lines beg end))))


(provide 'pmemo)

;;; pmemo.el ends here
