; 今週・先週変更したファイルに色をつける
;
; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=wcolorful%20dired

(defface face-file-edited-today
  '((((class color)
      (background dark))
     (:foreground "GreenYellow"))
    (((class color)
      (background light))
     (:foreground "magenta"))
    (t
     ())) nil)
(defface face-file-edited-this-week
  '((((class color)
      (background dark))
     (:foreground "LimeGreen"))
    (((class color)
      (background light))
     (:foreground "violet red"))
    (t
     ())) nil)
(defface face-file-edited-last-week
  '((((class color)
      (background dark))
     (:foreground "saddle brown"))
    (((class color)
      (background light))
     (:foreground "maroon"))
    (t
     ())) nil)
(defvar face-file-edited-today
  'face-file-edited-today)
(defvar face-file-edited-this-week
  'face-file-edited-this-week)
(defvar face-file-edited-last-week
  'face-file-edited-last-week)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (concat "\\(" (format-time-string "%b %e" (current-time))
           "\\|"(format-time-string "%m-%d" (current-time))
           "\\)"
           " [0-9]....") arg t))
(defun my-dired-date (time)
  "Fontlock search function for dired."
  (let ((now (current-time))
        (days (* -1 time))
        dateh datel daysec daysh daysl dir
        (offset 0))
    (setq daysec (* -1.0 days 60 60 24))
    (setq daysh (floor (/ daysec 65536.0)))
    (setq daysl (round (- daysec (* daysh 65536.0))))
    (setq dateh (- (nth 0 now) daysh))
    (setq datel (- (nth 1 now) (* offset 3600) daysl))
    (if (< datel 0)
        (progn
          (setq datel (+ datel 65536))
          (setq dateh (- dateh 1))))
    ;;(floor (/ offset 24))))))
    (if (< dateh 0)
        (setq dateh 0))
    ;;(insert (concat (int-to-string dateh) ":"))
    (list dateh datel)))
(defun my-dired-this-week-search (arg)
  "Fontlock search function for dired."
  (let ((youbi
         (string-to-int
          (format-time-string "%w" (current-time))))
        this-week-start this-week-end day ;;regexp
        (flg nil))
    (setq youbi (+ youbi 1))
    (setq regexp
          (concat "\\("))
    (while (not (= youbi 0))
      (setq regexp
            (concat
             regexp
             (if flg
                 "\\|")
             (format-time-string
              "%b %e"
              (my-dired-date youbi))
             "\\|"
             (format-time-string
              "%m-%d"
              (my-dired-date youbi))
             ))
      ;;(insert (concat (int-to-string youbi) "\n"))
      (setq flg t)
      (setq youbi (- youbi 1))))
  (setq regexp
        (concat regexp "\\)"))
  (search-forward-regexp
   (concat regexp " [0-9]....") arg t))
(defun my-dired-last-week-search (arg)
  "Fontlock search function for dired."
  (let ((youbi
         (string-to-int
          (format-time-string "%w" (current-time))))
        this-week-start this-week-end day ;;regexp
        lyoubi
        (flg nil))
    (setq youbi (+ youbi 0))
    (setq lyoubi (+ youbi 7))
    (setq regexp
          (concat "\\("))
    (while (not (= lyoubi youbi))
      (setq regexp
            (concat
             regexp
             (if flg
                 "\\|")
             (format-time-string
              "%b %e"
              (my-dired-date lyoubi))
             "\\|"
             (format-time-string
              "%m-%d"
              (my-dired-date lyoubi))
             ))
      ;;(insert (concat (int-to-string youbi) "\n"))
      (setq flg t)
      (setq lyoubi (- lyoubi 1))))
  (setq regexp
        (concat regexp "\\)"))
  (search-forward-regexp
   (concat regexp " [0-9]....") arg t))

(font-lock-add-keywords
 major-mode
 (list
  '(my-dired-today-search . face-file-edited-today)
  '(my-dired-this-week-search . face-file-edited-this-week)
  '(my-dired-last-week-search . face-file-edited-last-week)
  ))
