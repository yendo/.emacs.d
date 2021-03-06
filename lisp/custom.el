;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;画面を 2 分割したときの 上下を入れ替える
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=swap%20screen
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vzライクなウィンドウ分割

(defun vzlike-split-window (num)
  "split and unsplit window(s) like Vz Editor"
  (interactive "p")
  (if (equal (one-window-p) 't)
      (progn 
       (split-window)
       (switch-to-buffer-other-window (other-buffer))
       (other-window '1)
       )
    (delete-other-windows)))

(defun vzlike-change-buffer (num)
  "split and unsplit window(s) like Vz Editor"
  (interactive "p")
       (switch-to-buffer (other-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image 展開
;;
;; この関数は土屋雅稔さんが書いてくださった (ELF:01205)
;; <http://www-nagao.kuee.kyoto-u.ac.jp/member/tsuchiya/>
;; 
(defun image-file-name-completion (file predicate flag)
  "Completion function for image files."
  (let ((regexp "\\(jpg\\|png\\|gif\\)$"))
    (if (eq flag 'lambda)
	(and (string-match regexp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
      (let* ((dir (file-name-as-directory
		   (or (file-name-directory file)
		       default-directory)))
	     (collection
	      (delq nil
		    (mapcar
		     (lambda (f)
		       (unless (string-match "^\\.\\.?$" f)
			 (cond
			  ((file-directory-p (setq f (concat dir f)))
			   (list (file-name-as-directory f)))
			  ((string-match regexp f)
			   (list f)))))
		     (directory-files dir)))))
	(cond
	 ((not flag)
	  (try-completion file collection predicate))
	 ((eq flag t)
	  (all-completions file collection predicate)))))))

(defun insert-image-file (filename)
  (interactive 
   (list (completing-read "Image file: " 'image-file-name-completion
			  nil t (file-name-as-directory default-directory))))
  (let* ((filename (expand-file-name filename))
	 (image (create-image filename))
	 (url (concat "<" filename ">")))
    (insert-image image url)))
  
(defun expand-images ()
  (interactive)
  (let* ((pos (point))
	 (home (getenv "HOME"))
	 (regexp (format "<\\(%s/.*\\.\\(jpg\\|png\\|gif\\)\\)>" home)))
    (while (re-search-forward regexp nil t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (filename (match-string 1))
	     (image (cons 'image (cdr (create-image filename)))))
	(add-text-properties start end
			     (list 'display image
				   'intangible image
				   'rear-nonsticky (list 'display)))))
    (goto-char pos)))
  
; Emacs 21.1 で *.jpg ファイルを create-image できない問題の回避策

 (defadvice create-image (around create-image-ad activate)
   (let ((type (if (and (stringp file-or-data)
                     (string-match "\\.jpg\\'" file-or-data))
                'jpeg type)))
     ad-do-it))
