(defvar pmemo-command "~/bin/zmemo.pl")

(defun pmemo ()
  "Display each memo headers of Palm MemoDB."
  (interactive)
  (save-excursion
    (message "Executing memo headers...")
    (switch-to-buffer (get-buffer-create "*Palm Memo List*"))
    (setq buffer-read-only nil)
    (setq standard-output (current-buffer))
    (erase-buffer)
    (shell-command pmemo-command (current-buffer))
    (pmemo-mode)
    ))

(defun pmemo-mode ()
  "Major mode for viewing palm memo database.

f -- Find a palm memo at point.
q -- Quit."
  (kill-all-local-variables)

  (setq buffer-read-only t)
  (setq major-mode 'pmemo-mode
	mode-name "pmemo list")
  (setq pmemo-mode-map (make-keymap))
  (suppress-keymap pmemo-mode-map t)
  (define-key pmemo-mode-map "q" 'kill-this-buffer)

  (define-key pmemo-mode-map "f" 'pmemo-f)
  (define-key pmemo-mode-map "v" 'pmemo-f)

  (use-local-map pmemo-mode-map)
  (message "Commands: f, j, e, m; q to quit.")
;  (run-hooks 'debian-www-checklist-mode-hook)
  )





(defun pmemo-f ()
  "Display each memo headers of Palm MemoDB."
  (interactive)

  (let ((resize-mini-windows nil))
  (save-excursion

    (beginning-of-line)
    (re-search-forward "^\\([0-9]*\\) " nil nil)
    (setq num
	  (buffer-substring (match-beginning 1) (match-end 1)))


;    (message "Executing memo headers...")
    (switch-to-buffer (get-buffer-create "*Palm Memo*"))
    (setq buffer-read-only nil)
    (setq standard-output (current-buffer))
    (erase-buffer)
    (shell-command (concat pmemo-command " " num) (current-buffer))
    (goto-line 1)
 ;   (message "Executing memo headers...")
    (pmemo-mode)
    )
))

