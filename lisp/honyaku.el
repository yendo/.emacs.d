;;;
;;; honyaku.el --- Honyakudamashii client
;;;

;; Copyright (C) 1999 OMRON SOFTWARE Co., Ltd.

;; Author: OMRON SOFTWARE Co., Ltd. <honyaku-info@omronsoft.co.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Honyakudamashii client can be used on GNU Emacs with loading this file on it.

;;; Change log:

;;; 1999/10/04 V1.02
;;;          sentence access method modified
;;; 1999/09/29 V1.01
;;;          window access method modified
;;; 1999/09/10 V1.00
;;;          First Release

;;; Code:

;;; Last modified date: 1999/10/04

(defvar honyaku-version "1.02"
  "Version number of Honyakudamashii client.")

(defun honyaku-version ()
  (interactive)
  (message (format "Honyakudamashii client version %s" honyaku-version)))


(defconst honyaku-id-ej 1 "ejの識別コード")
(defconst honyaku-id-je 2 "jeの識別コード")

(defconst HONYAKU_SERVER_PORT   2744 "デフォルトサーバポート")
(defconst HONYAKU_LOCK_WAITTIME   30 "デフォルト辞書ファイルロックタイムアウト値")
(defconst HONYAKU_TRAN_WAITTIME   30 "デフォルト翻訳タイムアウト値")
(defconst HONYAKU_SCREEN_MIN_WIDTH  40 "画面の幅の最小値")
(defconst HONYAKU_SCREEN_MIN_HEIGHT 12 "画面の高さの最小値")

(defconst ewnn-word-separator " (){}<>[]/|\"@\\\t\n\.\,　"
  "区切り文字に使う文字")
(defconst ewnn-word-separator-regexp "[][ (){}<>/|\"@\\\t\n\.\,　]"
  "区切り文字に使う文字の正規表現")

(defconst honyaku-translation-buffer-name "*honyaku-translation*" "翻訳バッファ名")
(defconst honyaku-candidate-buffer-name   "*honyaku-candidate*"   "候補バッファ名")
(defconst honyaku-dictionary-buffer-name  "*honyaku-dictionary*"  "辞書バッファ名")


(define-key global-map [f9]     'honyaku-translate-ej)
(define-key global-map "\C-xye" 'honyaku-translate-ej)

(define-key global-map "\C-xyw" 'honyaku-word-translate-ej)
(define-key global-map "\C-xys" 'honyaku-sentence-translate-ej)
(define-key global-map "\C-xyp" 'honyaku-paragraph-translate-ej)

(define-key global-map [f10]    'honyaku-dic-word-ej)
(define-key global-map "\C-xyn" 'honyaku-dic-word-ej)

(define-key global-map "\C-xyi" 'honyaku-dic-ej)

(define-key global-map [f11]    'honyaku-translate-je)
(define-key global-map "\C-xyj" 'honyaku-translate-je)

(define-key global-map "\C-xyb" 'honyaku-sentence-translate-je)
(define-key global-map "\C-xyd" 'honyaku-paragraph-translate-je)

(define-key global-map [f12]    'honyaku-dic-je)
(define-key global-map "\C-xym" 'honyaku-dic-je)


(defvar honyaku-server-list nil "*サーバホスト名のリスト")
(defvar honyaku-server-name nil "*サーバホスト名")
(defvar honyaku-process nil "Process Number to HonyakuServer")
(defvar honyaku-active-server-name nil "接続中のサーバホスト名")
(defvar honyaku-active-server-port nil "接続中のサーバポート番号")
(defvar honyaku-default-server-port 2744 "honyakuサーバのデフォルトポート番号")

(defvar honyaku-initialized-ej nil "ej初期化済み")
(defvar honyaku-initialized-je nil "je初期化済み")

(defvar honyaku-rcfile-modestring-ej  nil "ejのモード設定値")
(defvar honyaku-rcfile-modestring-je  nil "jeのモード設定値")
(defvar honyaku-rcfile-hostname       nil "ホスト名")
(defvar honyaku-rcfile-port           nil "ポート番号")
(defvar honyaku-rcfile-use-compdic-ej nil "専門辞書のejでの使用")
(defvar honyaku-rcfile-use-compdic-je nil "専門辞書のjeでの使用")
(defvar honyaku-rcfile-use-learn      nil "学習辞書の使用")
(defvar honyaku-rcfile-lock-waittime  nil "辞書ファイルロックのタイムアウト値")
(defvar honyaku-rcfile-tran-waittime  nil "翻訳のタイムアウト値")
(defvar honyaku-rcfile-errors         nil "環境設定ファイルのエラー")

(defvar honyaku-connected-ej nil)
(defvar honyaku-connected-je nil)

(defun honyaku-server-connect-p ()
  "サーバと接続状態にあるかどうかを調べる。"
  (let ((status))
    (if (and honyaku-process
	     (setq status (process-status honyaku-process)))
	(if (or (equal 'open status) (equal 'run status))
	    t
	  nil)
      nil)))

(defun honyaku-connect-server (ej_or_je)
  "L-Transサーバとの接続処理を行なう。"
  (interactive)
  (honyaku-check-screen-size)
  (honyaku-make-connection-with-server)
  (if (not (honyaku-server-connect-p))
      (honyaku-error-log -300))
  (honyaku-initialize-server ej_or_je))

(defun honyaku-check-screen-size ()
  "画面のサイズをチェックする。"
  (if (or (< (frame-width) HONYAKU_SCREEN_MIN_WIDTH)
	  (< (frame-height) HONYAKU_SCREEN_MIN_HEIGHT))
      (honyaku-error-log -500)))

(defun honyaku-make-connection-with-server ()
  "サーバと接続する。接続の前に環境設定ファイルを読む。
honyaku-server-list, honyaku-server-name,
環境変数HONYAKUSERVER, 環境設定ファイルの順に接続先ホスト名を参照する"
  (if (not (honyaku-server-connect-p))
      (let* ((dummyresult (honyaku-read-and-set-variables-from-rcfile))
	     (envhost)
	     (hostlist (append honyaku-server-list
			       (list honyaku-server-name)
			       (list (progn
				       (setq envhost (getenv "HONYAKUSERVER"))
				       (if (and envhost
						(string-match "[:/]" envhost))
					   nil
					 envhost)))
			       (list (format "%s:%d" honyaku-rcfile-hostname
					     (- honyaku-rcfile-port
						honyaku-default-server-port)))
			       (list "localhost")))
	     (loginname (user-login-name)))
	(setq hostlist (delq nil hostlist))
	(setq honyaku-active-server-name nil)
	(catch 'success
	  (while hostlist
	    (let ( (hostname (car hostlist)) (poffset nil) )
	      (setq honyaku-active-server-port honyaku-default-server-port)
	      (if (setq poffset (string-match ":" hostname))
		  (progn
		    (setq honyaku-active-server-port
			  (+ honyaku-active-server-port
			     (string-to-number
			      (substring hostname (1+ poffset)))))
		    (setq hostname (substring hostname 0 poffset))))
	      (if (or (not hostname) (equal hostname ""))
		  (setq hostname "localhost"))
	      (if (setq honyaku-process (honyaku-com-start
					 hostname honyaku-active-server-port))
		  (progn
		    (setq honyaku-active-server-name hostname)
		    (throw 'success hostname)
		    (setq hostlist (cdr hostlist)))
		(setq hostlist (cdr hostlist)))))))))

(defun honyaku-disconnect-server ()
  "サーバとの接続を切断する。"
  (interactive)
  (if (honyaku-server-connect-p)
      (progn
	(if honyaku-initialized-ej
	    (progn
	      (honyaku_eg_end honyaku-process honyaku-id-ej)
	      (setq honyaku-initialized-ej nil)
	      (setq honyaku-connected-ej nil)))
	(if honyaku-initialized-je
	    (progn
	      (honyaku_eg_end honyaku-process honyaku-id-je)
	      (setq honyaku-initialized-je nil)
	      (setq honyaku-connected-je nil)))
	(kill-buffer (process-buffer honyaku-process))
	(delete-process honyaku-process))))

(defun honyaku-read-and-set-variables-from-rcfile ()
  "環境設定ファイルを読む。"
  (let ( (rc-contents (read-rc-file)) )
    (setq honyaku-rcfile-modestring-ej  (nth 0 rc-contents))
    (setq honyaku-rcfile-modestring-je  (nth 1 rc-contents))
    (setq honyaku-rcfile-hostname       (nth 2 rc-contents))
    (setq honyaku-rcfile-port           (nth 3 rc-contents))
    (setq honyaku-rcfile-use-compdic-ej (nth 4 rc-contents))
    (setq honyaku-rcfile-use-compdic-je (nth 5 rc-contents))
    (setq honyaku-rcfile-use-learn      (nth 6 rc-contents))
    (setq honyaku-rcfile-lock-waittime  (nth 7 rc-contents))
    (setq honyaku-rcfile-tran-waittime  (nth 8 rc-contents))
    (setq honyaku-rcfile-errors         (nth 9 rc-contents)))
  (if honyaku-rcfile-errors
      (let ( msgs onemsg )
	(setq msgs honyaku-rcfile-errors)
	(while msgs
	  (setq onemsg (car msgs))
	  (message onemsg)
	  (beep)
	  (sleep-for 1)
	  (setq msgs (cdr msgs))))))

(defun honyaku-initialize-server (ej_or_je)
  "初期化コマンドなどをサーバへ送る。"
  (let ( (result)
	 (loginname (user-login-name)))
    (if (or
	 (and (= ej_or_je honyaku-id-ej) honyaku-initialized-ej)
	 (and (= ej_or_je honyaku-id-je) honyaku-initialized-je))
	t
      (if (or 
	   (and (not honyaku-connected-ej) (= ej_or_je honyaku-id-ej))
	   (and (not honyaku-connected-je) (= ej_or_je honyaku-id-je)))
	  (progn
	    (setq result (honyaku_eg_init honyaku-process ej_or_je loginname))
	    (honyaku-error-log result "honyaku_eg_init")))

      (cond
       ((= ej_or_je honyaku-id-ej) (setq honyaku-connected-ej t))
       ((= ej_or_je honyaku-id-je) (setq honyaku-connected-je t)))


      (setq result (honyaku_eg_setmode honyaku-process ej_or_je
				       (cond
					((= ej_or_je honyaku-id-ej)
					 honyaku-rcfile-modestring-ej)
					((= ej_or_je honyaku-id-je)
					 honyaku-rcfile-modestring-je))))
      (honyaku-error-log result "honyaku_eg_setmode")

      (setq result (honyaku_eg_setdictname honyaku-process ej_or_je
					   (cond
					    ((= ej_or_je honyaku-id-ej)
					     honyaku-rcfile-use-compdic-ej)
					    ((= ej_or_je honyaku-id-je)
					     honyaku-rcfile-use-compdic-je))
					   honyaku-rcfile-use-learn))
      (honyaku-error-log result "honyaku_eg_setdictname")

      (setq result (honyaku_eg_clear_cache honyaku-process ej_or_je))
      (honyaku-error-log result "honyaku_eg_clear_cache")

      (setq result (honyaku_eg_set_timeout honyaku-process ej_or_je
					   honyaku-rcfile-lock-waittime
					   honyaku-rcfile-tran-waittime))
      (honyaku-error-log result "honyaku_eg_set_timeout")

      (cond
       ((= ej_or_je honyaku-id-ej) (setq honyaku-initialized-ej t))
       ((= ej_or_je honyaku-id-je) (setq honyaku-initialized-je t)))
      )))

(defun read-rc-file ()
  "環境設定ファイルを読み込み、その内容を返す。"
  (interactive)
  (save-excursion
    (let ( (rcbuf) (i) (p0) (p1) (keyword) (value) (error-messages)
	   (modestring_ej) (modestring_je)
	   (server_hostname)
	   (server_port) (server_port_i)
	   (use_computer_dic_ej)
	   (use_computer_dic_je)
	   (study_word_choice)
	   (ej_ignore_case)
	   (ej_preserve_input_order)
	   (ej_imperative_by_declarative)
	   (ej_use_polite_expression)
	   (ej_connect_katakana_by_point)
	   (je_not_by_apostroph)
	   (je_treatment_of_omitted_subject)
	   (je_translation_of_ing)
	   (je_omitted_subject)
	   (je_omitted_object)
	   (lock_waittime) (lock_waittime_i)
	   (tran_waittime) (tran_waittime_i)
	   (numline)
	   (fmt400
	    "設定ファイルの%d行目に無効なキーワードが設定されています(-400)")
	   (fmt401
	    "設定ファイルの%d行目に重複キーワード%sが設定されています(-401)")
	   (fmt402
	    "設定ファイルの%d行目に無効な値が設定されています(-402)")
	   )
      (if (not (file-readable-p "~/.honyakurc"))
	  (list
	   "KW"
	   "10110:I:0:it"
	   "localhost" HONYAKU_SERVER_PORT t t t
	   HONYAKU_LOCK_WAITTIME HONYAKU_TRAN_WAITTIME
	   nil)
	(setq rcbuf (generate-new-buffer "*honyaku-honyakurc*"))
	(set-buffer rcbuf)
	(insert-file-contents "~/.honyakurc")
	(setq numline (count-lines (point-min) (point-max)))
	(let ((i 0))
	  (while (< i numline)
	    (goto-line (1+ i))
	    (if (or (looking-at "#") (looking-at "\n") (looking-at " "))
		nil			; skip comment
	      (while (looking-at "[\t ]") (forward-char 1))
	      (setq p0 (point))
	      (while (looking-at "[a-zA-Z0-9_]") (forward-char 1))
	      (setq p1 (point))
	      (setq keyword (buffer-substring p0 p1))
	      (while (looking-at "[\t ]") (forward-char 1))
	      (setq p0 (point))
	      (while (looking-at "[a-zA-Z0-9_\-]") (forward-char 1))
	      (setq p1 (point))
	      (setq value (buffer-substring p0 p1))
	      (cond

	       ((string= keyword "server_hostname")
		(if (not server_hostname)
		    (setq server_hostname value)
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "server_hostname"))))))

	       ((string= keyword "server_port")
		(if (not server_port)
		    (if (or (< (string-to-number value) 0)
			    (> (string-to-number value) 65535)
			    (not (string-match "^[\-]?[0-9]+$" value))
			    (and (= (string-to-number value) 0)
				 (not (string-match "^[0]+$" value))))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (progn
			(setq server_port value)
			(setq server_port_i (string-to-number value))))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "server_port"))))))

	       ((string= keyword "use_computer_dic_ej")
		(if (not use_computer_dic_ej)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq use_computer_dic_ej value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "use_computer_dic_ej"))))))

	       ((string= keyword "use_computer_dic_je")
		(if (not use_computer_dic_je)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq use_computer_dic_je value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "use_computer_dic_je"))))))

	       ((string= keyword "study_word_choice")
		(if (not study_word_choice)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq study_word_choice value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "study_word_choice"))))))

	       ((string= keyword "ej_ignore_case")
		(if (not ej_ignore_case)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq ej_ignore_case value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "ej_ignore_case"))))))

	       ((string= keyword "ej_preserve_input_order")
		(if (not ej_preserve_input_order)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq ej_preserve_input_order value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "ej_preserve_input_order"))))))

	       ((string= keyword "ej_imperative_by_declarative")
		(if (not ej_imperative_by_declarative)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq ej_imperative_by_declarative value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "ej_imperative_by_declarative"))))))

	       ((string= keyword "ej_use_polite_expression")
		(if (not ej_use_polite_expression)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq ej_use_polite_expression value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "ej_use_polite_expression"))))))

	       ((string= keyword "ej_connect_katakana_by_point")
		(if (not ej_connect_katakana_by_point)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq ej_connect_katakana_by_point value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "ej_connect_katakana_by_point"))))))

	       ((string= keyword "je_not_by_apostroph")
		(if (not je_not_by_apostroph)
		    (if (not (or (string= value "true")
				 (string= value "false")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq je_not_by_apostroph value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "je_not_by_apostroph"))))))

	       ((string= keyword "je_treatment_of_omitted_subject")
		(if (not je_treatment_of_omitted_subject)
		    (if (not (or (string= value "0")
				 (string= value "1")
				 (string= value "2")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq je_treatment_of_omitted_subject value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "je_treatment_of_omitted_subject"))))))

	       ((string= keyword "je_translation_of_ing")
		(if (not je_translation_of_ing)
		    (if (not (or (string= value "0")
				 (string= value "1")
				 (string= value "2")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq je_translation_of_ing value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "je_translation_of_ing"))))))

	       ((string= keyword "je_omitted_subject")
		(if (not je_omitted_subject)
		    (if (not (or (string= value "0")
				 (string= value "1")
				 (string= value "2")
				 (string= value "3")
				 (string= value "4")
				 (string= value "5")
				 (string= value "6")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq je_omitted_subject value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "je_omitted_subject"))))))

	       ((string= keyword "je_omitted_object")
		(if (not je_omitted_object)
		    (if (not (or (string= value "0")
				 (string= value "1")
				 (string= value "2")
				 (string= value "3")
				 (string= value "4")
				 (string= value "5")
				 (string= value "6")
				 (string= value "7")
				 (string= value "8")))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (setq je_omitted_object value))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "je_omitted_object"))))))

	       ((string= keyword "lock_waittime")
		(if (not lock_waittime)
		    (if (or (< (string-to-number value) 0)
			    (>= (string-to-number value) 600)
			    (not (string-match "^[\-]?[0-9]+$" value))
			    (and (= (string-to-number value) 0)
				 (not (string-match "^[0]+$" value))))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (progn
			(setq lock_waittime value)
			(setq lock_waittime_i (string-to-number value))))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "lock_waittime"))))))

	       ((string= keyword "tran_waittime")
		(if (not tran_waittime)
		    (if (or (<= (string-to-number value) 0)
			    (>= (string-to-number value) 600)
			    (not (string-match "^[\-]?[0-9]+$" value)))
			(setq error-messages
			      (append error-messages
				      (list (format fmt402 (1+ i)))))
		      (progn
			(setq tran_waittime value)
			(setq tran_waittime_i (string-to-number value))))
		  (setq error-messages
			(append error-messages
				(list (format fmt401 (1+ i)
					      "tran_waittime"))))))

	       (t
		(setq error-messages
		      (append error-messages (list (format fmt400 (1+ i))))))

	       ))
	    (setq i (1+ i)))
	  (if (not server_hostname) (setq server_hostname "localhost"))
	  (if (not server_port_i) (setq server_port_i HONYAKU_SERVER_PORT))
	  (if (not use_computer_dic_ej) (setq use_computer_dic_ej "true"))
	  (if (not use_computer_dic_je) (setq use_computer_dic_je "true"))
	  (if (not study_word_choice) (setq study_word_choice "true"))
	  (if (not ej_ignore_case) (setq ej_ignore_case "false"))
	  (if (not ej_preserve_input_order)
	      (setq ej_preserve_input_order "false"))
	  (if (not ej_imperative_by_declarative)
	      (setq ej_imperative_by_declarative "false"))
	  (if (not ej_use_polite_expression)
	      (setq ej_use_polite_expression "false"))
	  (if (not ej_connect_katakana_by_point)
	      (setq ej_connect_katakana_by_point "true"))
	  (if (not je_not_by_apostroph) (setq je_not_by_apostroph "false"))
	  (if (not je_treatment_of_omitted_subject)
	      (setq je_treatment_of_omitted_subject "0"))
	  (if (not je_translation_of_ing) (setq je_translation_of_ing "0"))
	  (if (not je_omitted_subject) (setq je_omitted_subject "0"))
	  (if (not je_omitted_object) (setq je_omitted_object "0"))
	  (if (not lock_waittime_i) (setq lock_waittime_i HONYAKU_LOCK_WAITTIME))
	  (if (not tran_waittime_i) (setq tran_waittime_i HONYAKU_TRAN_WAITTIME))
	  (setq modestring_ej
		(format
		 "%s%s%s%s%s%s"
		 (if (string= ej_ignore_case               "true") "C" "")
		 (if (string= ej_preserve_input_order      "true") "O" "")
		 (if (string= ej_imperative_by_declarative "true") "I" "")
		 (if (string= ej_use_polite_expression     "true") "D" "")
		 (if (string= ej_connect_katakana_by_point "true") "K" "")
		 (if (string= study_word_choice            "true") "W" "")))
	  (setq modestring_je
		(format
		 "%s%s%d%d0:%s:0:%s"
		 (if (string= study_word_choice "true") "1" "0")
		 (if (string= je_not_by_apostroph "true") "1" "0")
		 (1+ (string-to-number je_treatment_of_omitted_subject))
		 (1+ (string-to-number je_translation_of_ing))
		 (cond
		  ((string= je_omitted_subject "0") "I")
		  ((string= je_omitted_subject "1") "you")
		  ((string= je_omitted_subject "2") "it")
		  ((string= je_omitted_subject "3") "he")
		  ((string= je_omitted_subject "4") "she")
		  ((string= je_omitted_subject "5") "we")
		  ((string= je_omitted_subject "6") "they"))
		 (cond
		  ((string= je_omitted_object "0") "it")
		  ((string= je_omitted_object "1") "you")
		  ((string= je_omitted_object "2") "them")
		  ((string= je_omitted_object "3") "us")
		  ((string= je_omitted_object "4") "me")
		  ((string= je_omitted_object "5") "him")
		  ((string= je_omitted_object "6") "her")
		  ((string= je_omitted_object "7") "that")
		  ((string= je_omitted_object "8") ","))
		 )))
	(kill-buffer rcbuf)
	(list
	 modestring_ej
	 modestring_je
	 server_hostname
	 server_port_i
	 (if (string= use_computer_dic_ej "true") t nil)
	 (if (string= use_computer_dic_je "true") t nil)
	 (if (string= study_word_choice "true") t nil)
	 lock_waittime_i
	 tran_waittime_i
	 error-messages)
	))))

(add-hook 'kill-emacs-hook 'honyaku-disconnect-server t)

(defun ewnn-get-word-on-point (&optional not-check-word)
  (save-excursion 
    (let ((wstart nil)(wend nil))
      (catch 'exit
	(if (or (eobp) (looking-at ewnn-word-separator-regexp))
	    (progn
	      (if (not (bobp))
		  (forward-char -1)
		(throw 'exit nil)))
	  (progn 
	    (skip-chars-backward ewnn-word-separator)
	    (if (looking-at ewnn-word-separator-regexp)
		(skip-chars-forward ewnn-word-separator))))
	(while (and (not (bobp))(not (looking-at ewnn-word-separator-regexp)))
	  (forward-char -1))
	(if (and (not (bobp)) (not (eobp)))
	    (forward-char 1)
	  (if (and (bobp) (looking-at ewnn-word-separator-regexp))
	      (forward-char 1)))
	(if (and (<= (point-min) (point)) (<= (point) (point-max)))
	    (progn
	      (setq wstart (point))
	      (setq wend (progn 
			   (while (and (not (eobp))
				       (not (looking-at ewnn-word-separator-regexp)))
			     (forward-char 1))
			   (point)))
	      (if not-check-word ()
		(setq ewnn-check-word-start wstart)
		(setq ewnn-check-word-end wend))
	      (buffer-substring wstart wend))
	  nil)))))

(defconst honyaku-message-window "*honyaku-message*")

(defun honyaku-error-log (stat &optional info in-translation)
  (save-excursion
    (if (and (integerp stat) (< stat 0))
	(let ((message-buff (get-buffer-create honyaku-message-window))
	      (message) (msg))
	  (set-buffer message-buff)
	  (goto-char (point-max))
	  (setq buffer-read-only nil)
	  (setq msg (get-error-message stat))
	  (setq message (format "HONYAKU: %s(%d)" msg stat))
	  (insert (substring (current-time-string) 4 19) " " message)
	  (if info (insert (format " (%s)" info)))
	  (insert ?\n)
	  (setq buffer-read-only t)
	  (bury-buffer message-buff)
	  (if in-translation
	      (message "%s" message)
	    (error "%s" message))))))

(defun get-error-message (stat)
  (or
   (nth 1 (assoc stat honyaku-message-alist))
   "■未定義のエラーコードです■"))

(defun get-translation-error-message (stat)
  (format "【%s】" (get-error-message stat)))

(defvar honyaku-message-alist
  '(
    (-206  "原文または訳文が長すぎます。文を短く切って再翻訳してください。")
    (-300  "サーバと接続できませんでした。")
    (-500  "ウィンドウのサイズが小さ過ぎます。")
    (-501  "サーバとの通信で致命的エラーが発生しました。")
    (-502  "指定された位置には単語が存在しません。")
    (-503  "訳語候補がありません。")
    (-600  "ライセンスが取得できませんでした。")
    (-1003 "構文解析に失敗しました。原文を修正して再翻訳してください。")
    (-1012 "原文のその位置には単語が存在しません")
    (-3000 "翻訳で時間がかかり過ぎてタイムアウトが発生しました。")
    (-3004 "同一ユーザの学習辞書のロックでタイムアウトが発生しました。")
    (-4000 "原文または訳文が長すぎます。文を短く切って再翻訳してください。")
    (-4002 "翻訳結果が正しくありません。専門用語辞書の設定を確認してください。")
    (-4003 "翻訳結果が正しくありません。専門用語辞書の設定を確認してください。")
    (-4004 "まだ翻訳されていません。")
    (-4016 "辞書に指定した単語がありません。")
    (-4017 "辞書の読み込みができませんでした。")
    ))

(defun honyaku-warning (message)
  (let ( (outmsg) )
    (setq outmsg (format "HONYAKU: %s" message))
    (error "%s" outmsg)))

(defun honyaku-output-msg (message &rest args)
  (message "%s" (format "HONYAKU: %s" (apply 'format (cons message args)))))


(defvar honyaku-yaku-mode-map nil "翻訳モードマップ")
(if honyaku-yaku-mode-map
    nil
  (setq honyaku-yaku-mode-map (make-sparse-keymap))
  (let ((ch 32))
    (while (<= ch 127)
      (define-key honyaku-yaku-mode-map (char-to-string ch) 'undefined)
      (setq ch (1+ ch))))
  (define-key honyaku-yaku-mode-map "n"    'next-line)
  (define-key honyaku-yaku-mode-map "p"    'previous-line)
  (define-key honyaku-yaku-mode-map " "    'honyaku-get-yakugo)
  (define-key honyaku-yaku-mode-map [tab]  'honyaku-get-hinshi)
  (define-key honyaku-yaku-mode-map "\C-i" 'honyaku-get-hinshi)
  (define-key honyaku-yaku-mode-map "q"    'honyaku-quit))

(defvar honyaku-koho-mode-map nil "候補モードマップ")
(if honyaku-koho-mode-map
    nil
  (setq honyaku-koho-mode-map (make-sparse-keymap))
  (let ((ch 32))
    (while (<= ch 127)
      (define-key honyaku-koho-mode-map (char-to-string ch) 'undefined)
      (setq ch (1+ ch))))
  (define-key honyaku-koho-mode-map "n"      'next-line)
  (define-key honyaku-koho-mode-map "p"      'previous-line)
  (define-key honyaku-koho-mode-map " "      'honyaku-next-candidate)
  (define-key honyaku-koho-mode-map [return] 'honyaku-get-koho)
  (define-key honyaku-koho-mode-map "\C-m"   'honyaku-get-koho)
  (define-key honyaku-koho-mode-map "q"      'honyaku-quit))

(defvar honyaku-dict-mode-map nil "辞書引きモードマップ")
(if honyaku-dict-mode-map
    nil
  (setq honyaku-dict-mode-map (make-sparse-keymap))
  (let ((ch 32))
    (while (<= ch 127)
      (define-key honyaku-dict-mode-map (char-to-string ch) 'undefined)
      (setq ch (1+ ch))))
  (define-key honyaku-dict-mode-map "q" 'honyaku-quit))

(defun honyaku-yaku-mode ()
  (kill-all-local-variables)
  (setq mode-name "honyaku-Translation")
  (setq major-mode 'honyaku-yaku-mode)
  (use-local-map honyaku-yaku-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun honyaku-koho-mode ()
  (kill-all-local-variables)
  (setq mode-name "honyaku-Translation")
  (setq major-mode 'honyaku-koho-mode)
  (use-local-map honyaku-koho-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun honyaku-dict-mode ()
  (kill-all-local-variables)
  (setq mode-name "honyaku-Dictionary")
  (setq major-mode 'honyaku-dict-mode)
  (use-local-map honyaku-dict-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun honyaku-translate-ej (start end)
  "英文の翻訳。[f9]"
  (interactive "r")
  (if (honyaku-translate-window-isimpossible)
      (honyaku-warning "このバッファでは翻訳を行なうことはできません。")
    (honyaku-translate (buffer-substring start end) honyaku-id-ej)))

(defun honyaku-translate-je (start end)
  "和文の翻訳。[f11]"
  (interactive "r")
  (if (honyaku-translate-window-isimpossible)
      (honyaku-warning "このバッファでは翻訳を行なうことはできません。")
    (honyaku-translate (buffer-substring start end) honyaku-id-je)))

(defun honyaku-translate (target-string ej_or_je)
  "複数の文を翻訳する。"
  (interactive)
  (honyaku-connect-server ej_or_je)
  (let ( (result) (status) (numsen) (sentences)
	 (cur-buf (current-buffer))
	 (cur-buf-nam (buffer-name))
	 (fmtstr
	  (cond
	   ((= ej_or_je honyaku-id-ej) "[英] %s\n[日] %s\n")
	   ((= ej_or_je honyaku-id-je) "[日] %s\n[英] %s\n"))))
    (if (string= target-string "")
	(honyaku-warning "空の文字列は翻訳できません。"))
    (setq result (honyaku_divide_sentence honyaku-process ej_or_je target-string))
    (honyaku-error-log result "honyaku_divide_sentence")
    (honyaku-output-msg "翻訳中です。")
    (get-buffer-create honyaku-translation-buffer-name)
    (set-buffer honyaku-translation-buffer-name)
    (or (eq major-mode 'honyaku-yaku-mode) (honyaku-yaku-mode))
    (setq mode-line-buffer-identification '("HONYAKU: " "翻訳結果"))
    (get-buffer-create honyaku-candidate-buffer-name)
    (set-buffer honyaku-candidate-buffer-name)
    (or (eq major-mode 'honyaku-koho-mode) (honyaku-koho-mode))
    (set-buffer cur-buf)
    (honyaku-split-for-yaku t)
    (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
    (setq status    (nth 0 result))
    (setq numsen    (nth 1 result))
    (setq sentences (nth 2 result))
    (select-window (get-buffer-window honyaku-translation-buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ( (i 0) (ppbegin 0) (ppend) (divided-string) (info-string)
	   (onesentence) (translated-string) )
      (while (< i numsen)
	(setq ppend (string-match "\n" sentences ppbegin))
	(setq divided-string (substring sentences ppbegin ppend))
	(setq onesentence    (substring sentences (+ 2 ppbegin) ppend))
	(setq info-string    (substring sentences ppbegin (+ 2 ppbegin)))
	(if (string-match "^!" info-string)
	    (setq translated-string (get-translation-error-message -206))
	  (setq result (honyaku_eg_translate_one honyaku-process
						 ej_or_je onesentence))
	  (honyaku-error-log result "honyaku_eg_translate_one" t)
	  (if (integerp result)
	      (setq translated-string (get-translation-error-message result))
	    (setq translated-string result)))
	(insert (format fmtstr onesentence translated-string))
	(setq ppbegin (1+ ppend))
	(if (< (1+ i) numsen) (insert "\n"))
	(setq i (1+ i))))
    (if (<= numsen 0)
	(progn
	  (honyaku-delete-yaku-window)
	  (honyaku-warning "空白などから成る文字列は翻訳できません。"))
      (put-text-property 1 2 'honyaku-tprop-translation (list ej_or_je))
      (setq buffer-read-only t)
      (goto-char (point-min)) (forward-char 4)
      (honyaku-output-msg "翻訳が終了しました。"))
    ))

(defun honyaku-word-translate-ej ()
  "単語の一発英日翻訳"
  (interactive)
  (if (honyaku-translate-window-isimpossible)
      (honyaku-warning "このバッファでは翻訳を行なうことはできません。")
    (let ( (target-string (ewnn-get-word-on-point)) )
      (if (not target-string)
	  (honyaku-error-log -502))
      (honyaku-translate target-string honyaku-id-ej))))

(defun honyaku-sentence-translate-ej ()
  "文の一発英日翻訳"
  (interactive)
  (if (honyaku-translate-window-isimpossible)
      (honyaku-warning "このバッファでは翻訳を行なうことはできません。")
    (honyaku-sentence-translate honyaku-id-ej)))

(defun honyaku-sentence-translate-je ()
  "文の一発日英翻訳"
  (interactive)
  (if (honyaku-translate-window-isimpossible)
      (honyaku-warning "このバッファでは翻訳を行なうことはできません。")
    (honyaku-sentence-translate honyaku-id-je)))

(defun honyaku-sentence-translate (ej_or_je)
  "文の一発英日または日英翻訳"
  (interactive)
  (let ((sentence-end-backup)
	(target-string))
    (save-excursion
      (if (>= emacs-major-version 20)
	  (progn
	    (setq sentence-end-backup sentence-end)
	    (setq sentence-end
		  (concat
		   "\\("
		   "\\("
		   "[.?!][]\"')}]*"
		   "\\|"
		   "[．？！][］”’）｝〕〉》」』]*"
		   "\\)"
		   "\\($\\|\t\\|  \\)"
		   "\\|"
		   "。"
		   "\\)"
		   "[ \t\n]*"))))
      
      (if (not (eobp)) 
	  (forward-sentence))
      (set-mark (point))
      (backward-sentence)
      (if (>= emacs-major-version 20)
	  (setq sentence-end sentence-end-backup))
      (setq target-string (buffer-substring (point) (mark))))
    (honyaku-translate target-string ej_or_je)))

(defun honyaku-paragraph-translate-ej ()
  "パラグラフの一発英日翻訳"
  (interactive)
  (if (honyaku-translate-window-isimpossible)
        (honyaku-warning "このバッファでは翻訳を行なうことはできません。")
    (let ((target-string))
      (save-excursion
	(mark-paragraph)
	(setq target-string (buffer-substring (point) (mark))))
      (honyaku-translate  target-string honyaku-id-ej))))

(defun honyaku-paragraph-translate-je ()
  "パラグラフの一発日英翻訳"
  (interactive)
  (if (honyaku-translate-window-isimpossible)
        (honyaku-warning "このバッファでは翻訳を行なうことはできません。")
    (let ((target-string))
      (save-excursion
	(mark-paragraph)
	(setq target-string (buffer-substring (point) (mark))))
      (honyaku-translate target-string  honyaku-id-je))))

(defun honyaku-translate-window-isimpossible ()
  "ウィンドウのチェック"
  (interactive)
  (or (string= (buffer-name) honyaku-translation-buffer-name)
      (string= (buffer-name) honyaku-candidate-buffer-name)
      (string= (buffer-name) honyaku-dictionary-buffer-name)))

(defun honyaku-dic-word-ej ()
  "英和の辞書引きを行なう。"
  (interactive)
  (honyaku-dic (ewnn-get-word-on-point) honyaku-id-ej))

(defun honyaku-dic-ej (start end)
  "英和の辞書引きを行なう。"
  (interactive "r")
  (honyaku-dic (buffer-substring start end) honyaku-id-ej))

(defun honyaku-dic-je (start end)
  "和英の辞書引きを行なう。"
  (interactive "r")
  (honyaku-dic (buffer-substring start end) honyaku-id-je))

(defun honyaku-dic (targetword ej_or_je)
  "辞書引きを行なう。"
  (interactive)
  (if targetword
      (if (string= targetword "")
	  (honyaku-error-log -502))
    (honyaku-error-log -502))
  (save-excursion
    (honyaku-connect-server ej_or_je)
    (let ( (result) (curbuf (current-buffer)) )
      (get-buffer-create honyaku-dictionary-buffer-name)
      (set-buffer honyaku-dictionary-buffer-name)
      (or (eq major-mode 'honyaku-dict-mode) (honyaku-dict-mode))
      (setq result (honyaku_eg_browse_dict honyaku-process ej_or_je targetword))
      (honyaku-error-log result "honyaku_eg_browse_dict")
      (honyaku-split-for-dict curbuf)
      (set-buffer honyaku-dictionary-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (cond
       ((= ej_or_je honyaku-id-ej)
	(setq mode-line-buffer-identification '("HONYAKU: " "英和辞書")))
       ((= ej_or_je honyaku-id-je)
	(setq mode-line-buffer-identification '("HONYAKU: " "和英辞書"))))
      (insert result)
      (setq buffer-read-only t)
      (select-window (get-buffer-window honyaku-dictionary-buffer-name)))))

(defun honyaku-get-yakugo()
  "訳語候補を表示する"
  (interactive)
  (honyaku-list-yakugo-or-hinshi t))

(defun honyaku-get-hinshi()
  "品詞候補を表示する"
  (interactive)
  (honyaku-list-yakugo-or-hinshi nil))

(defun honyaku-list-yakugo-or-hinshi (isyakugo)
  "翻訳結果バッファでカーソル位置の単語の訳語または品詞候補を表示する。"
  (let* ((translation-info (get-text-property 1 'honyaku-tprop-translation))
	 (ej_or_je (nth 0 translation-info)) (currp (point)) (debugging)
	 startp result fmtstr wordno koho-begin koho-lengt
	 sent-begin sent-end sent-tran langchar inspos numkoho numc)
    (save-excursion
      (if (= ej_or_je honyaku-id-ej)
	  (progn
	    (setq langchar "英")
	    (if isyakugo
		(setq fmtstr "\"%s\"の訳語候補:\n")
	      (setq fmtstr   "\"%s\"の品詞候補:\n")))
	(setq langchar "日")
	(setq fmtstr "『%s』の訳語候補:\n"))

      (if (and (not isyakugo) (= ej_or_je honyaku-id-je))
	  (progn
	    (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	    (honyaku-warning "日英翻訳で品詞候補の表示はできません。")))

      (if (eobp)
	  (progn
	    (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	    (honyaku-warning "原文の行ではありません")))

      (forward-line 1) (setq inspos (point))
      (forward-line -1)

      (if (>= emacs-major-version 20)
	  (setq numc 2)
	(setq numc 4))
      (if (not (string-match (format "^\\[%s" langchar)
			     (buffer-substring (point) (+ (point) numc))))
	  (progn
	    (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	    (honyaku-warning "原文の行ではありません")))

      (if (string= (char-to-string (char-after (point))) "\n")
	  (progn
	    (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	    (honyaku-warning "原文の行ではありません")))

      (forward-char 4)
      (setq sent-begin (point))
      (end-of-line) (setq sent-end (point))
      (setq sent-tran (buffer-substring sent-begin sent-end))
      (goto-char currp)

      (if (or (< currp sent-begin) (<= sent-end currp))
	  (progn
	    (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	    (honyaku-warning "原文の位置ではありません")))

      (setq result (honyaku_eg_translate_one honyaku-process ej_or_je sent-tran))
      (honyaku-error-log result "honyaku_eg_translate_one")

      (setq startp (honyaku-euc-strlen (buffer-substring sent-begin currp)))
      (setq result (honyaku_eg_getequiv honyaku-process ej_or_je 0 startp))
      (honyaku-error-log result "honyaku_eg_getequiv")
      (setq wordno (car result))
      (setq koho-begin (nth 3 result))
      (setq koho-lengt (nth 4 result))

      (if debugging
	  (progn
	    (honyaku-output-msg
	     "*** sent-begin:%d currp:%d ... startp:%d / wordno:%d inspos:%d"
	     sent-begin currp startp wordno inspos)
	    (honyaku-output-msg "*** getequiv-result:%s" result)))

      (if (not (get-buffer-window honyaku-candidate-buffer-name))	
	  (honyaku-split-for-yaku t))
      (set-buffer honyaku-candidate-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (if isyakugo
	  (setq mode-line-buffer-identification '("HONYAKU: " "訳語候補"))
	(setq mode-line-buffer-identification '("HONYAKU: " "品詞候補")))
      (insert (format fmtstr (honyaku-euc-substring sent-tran koho-begin
						    (+ koho-begin koho-lengt))))
      (put-text-property 1 2 'honyaku-tprop-koho
			 (list ej_or_je isyakugo wordno inspos))
      (if isyakugo
	  (progn
	    (setq result (honyaku_eg_getword honyaku-process ej_or_je wordno 4096 1))
	    (honyaku-error-log result "honyaku_eg_getword"))
	(setq result (honyaku_eg_gethinshi honyaku-process ej_or_je wordno 4096))
	(honyaku-error-log result "honyaku_eg_gethinshi"))
      (setq numkoho (car result))
      (setq result (cdr result))

      (if (= numkoho 0)
	  (progn
	    (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	    (honyaku-error-log -503)))
      (let ((i 0))
	(while (< i numkoho)
	  (insert (format "\t%d. %s\n" (1+ i) (substring (car result) 2)))
	  (setq result (cdr result))
	  (setq i (1+ i))))
      (setq buffer-read-only t))
    (select-window (get-buffer-window honyaku-candidate-buffer-name))
    (forward-line 1)
    (if (> numkoho 0)
	(forward-char))))

(defun honyaku-get-koho ()
  "訳語または品詞候補バッファでの単語選択を受けて再翻訳を行なう。"
  (interactive)
  (honyaku-translate-again))

(defun honyaku-translate-again ()
  (interactive)
  (setq buffer-read-only nil)
  (let* (
	 (equiv-info (get-text-property 1 'honyaku-tprop-koho))
	 (ej_or_je (nth 0 equiv-info))
	 (isyakugo (nth 1 equiv-info))
	 (wordno   (nth 2 equiv-info))
	 (inspos   (nth 3 equiv-info))
	 (yakuno) (po) (yakubun) (result) (debugging)
	 (fmtstr
	  (cond
	   ((= ej_or_je honyaku-id-ej) "[日] %s\n")
	   ((= ej_or_je honyaku-id-je) "[英] %s\n")
	   )))
    (save-excursion
      (if (eobp) (honyaku-warning "カーソルが候補の行にありません。"))
      (beginning-of-line) (forward-char) (setq po (point))
      (if (search-forward "." nil t)
	  (progn
	    (setq yakuno (string-to-number (buffer-substring (1- (point)) po)))
	    (if debugging
		(honyaku-output-msg
		 (format "*** honyaku-tprop-koho: %s" equiv-info)))
	    (if (<= yakuno 0) (honyaku-warning "カーソルが候補の行にありません。")
	      (setq result (honyaku_eg_setlearn honyaku-process ej_or_je 1))
	      (honyaku-error-log result "honyaku_eg_setlearn")
	      (if isyakugo
		  (progn
		    (setq result (honyaku_eg_setword honyaku-process ej_or_je
						     wordno (1- yakuno)))
		    (honyaku-error-log result "honyaku_eg_setword"))
		(setq result (honyaku_eg_sethinshi honyaku-process ej_or_je
						   wordno (1- yakuno)))
		(honyaku-error-log result "honyaku_eg_sethinshi"))
	      (setq result (honyaku_eg_gettrntxt honyaku-process ej_or_je))
	      (honyaku-error-log result "honyaku_eg_gettrntxt" t)
	      (if (integerp result)
		  (setq yakubun (get-translation-error-message result))
		(setq yakubun result))
	      (select-window (get-buffer-window honyaku-translation-buffer-name))
	      (goto-char inspos)
	      (setq buffer-read-only nil)
	      (insert (format fmtstr yakubun))
	      (setq buffer-read-only t)
	      (forward-line -1) (forward-char 4)
	      (delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	      ))
	(honyaku-warning "カーソルが候補の行にありません。")))))

(defun honyaku-next-candidate ()
  "候補バッファにおいて次の行へ移動する。"
  (interactive)
  (if (eobp)
      (goto-line 2)
    (forward-line 1)
    (if (eobp) (goto-line 2)))
  (forward-char 1))

(defun honyaku-quit ()
  "翻訳処理を終了する。ソケットはクローズしない"
  (interactive)
  (if (equal (current-buffer) (get-buffer honyaku-candidate-buffer-name))
      (progn
	(delete-windows-on (get-buffer honyaku-candidate-buffer-name))
	(select-window (get-buffer-window honyaku-translation-buffer-name)))
    (if (and
	 (equal (current-buffer) (get-buffer honyaku-dictionary-buffer-name))
	 (or
	  (get-buffer-window honyaku-translation-buffer-name)
	  (get-buffer-window honyaku-candidate-buffer-name)))
	(progn
	  (delete-windows-on (get-buffer honyaku-dictionary-buffer-name))
	  (if (get-buffer-window honyaku-candidate-buffer-name)
	      (select-window (get-buffer-window honyaku-candidate-buffer-name))
	    (if (get-buffer-window honyaku-translation-buffer-name)
		(select-window (get-buffer-window honyaku-translation-buffer-name)
			       ))))
      (honyaku-delete-yaku-window)
      (honyaku-delete-dict-window)
      (get-buffer-create honyaku-translation-buffer-name)
      (get-buffer-create honyaku-candidate-buffer-name)
      (get-buffer-create honyaku-dictionary-buffer-name)
      (kill-buffer honyaku-translation-buffer-name)
      (kill-buffer honyaku-candidate-buffer-name)
      (kill-buffer honyaku-dictionary-buffer-name))))

(defun honyaku-euc-strlen (string)
  (length
   (if (>= emacs-major-version 20)
       (encode-coding-string string 'euc-jp)
     (code-convert-string string '*internal* '*euc-japan*))))

(defun honyaku-euc-substring (string begin end)
  (let ((eucstr) (subeuc) (intstr))
    (setq eucstr
	  (if (>= emacs-major-version 20)
	      (encode-coding-string string 'euc-jp)
	    (code-convert-string string '*internal* '*euc-japan*)))
    (setq subeuc (substring eucstr begin end))
    (if (>= emacs-major-version 20)
	(decode-coding-string subeuc 'euc-jp)
      (code-convert-string subeuc '*euc-japan* '*internal*))
    ))


(defvar honyaku-previous-config nil "保存しておく window-configuration")
(defvar honyaku-yaku-return-window nil "翻訳の戻り先ウィンドウ")
(defvar honyaku-dict-return-window nil "辞書引きの戻り先ウィンドウ")
(defconst honyaku-window-min 3 "最小ウィンドウ高")
(defvar honyaku-split-rate 70 "*ウィンドウ使用率")
(setq window-min-height 2)

(defun honyaku-split-for-yaku (delete-dicwin)
  (let ( (dict-win (get-buffer-window honyaku-dictionary-buffer-name))
	 (yaku-win (get-buffer-window honyaku-translation-buffer-name))
	 (koho-win (get-buffer-window honyaku-candidate-buffer-name))
	 (next-height) (win-height) (height) (line-cnt) (lsize) )
    (setq honyaku-yaku-return-window (selected-window))
    (if (and yaku-win koho-win)
	(if delete-dicwin
	    (if dict-win (delete-windows-on (window-buffer dict-win))))
      (cond
       ((or yaku-win koho-win)
	(if yaku-win (delete-windows-on (window-buffer yaku-win)))
	(if koho-win (delete-windows-on (window-buffer koho-win)))
	(if delete-dicwin
	    (if dict-win (delete-windows-on (window-buffer dict-win))))
	(setq height (honyaku-get-size-split-rate))
	(split-window (selected-window) height)
	(set-window-buffer (other-window 1) honyaku-translation-buffer-name)
	(setq win-height (+ (* 2 honyaku-window-min) 2))
	(setq lsize (max 0 (- win-height (window-height (selected-window))))))
       (t
	(if (not honyaku-previous-config)
	    (setq honyaku-previous-config (current-window-configuration)))
	(delete-other-windows)
	(setq height (honyaku-get-size-split-rate))
	(if (and (eq emacs-major-version 19) (eq emacs-minor-version 34))
	    (progn
	      (if (bolp)
		  (setq line-cnt (1+ (count-lines (window-start) (point))))
		(setq line-cnt (count-lines (window-start) (point))))
	      (if (> line-cnt (1- height))
		  (recenter (/ (1- height) 2)))))
	(split-window (selected-window) height)
	(set-window-buffer (other-window 1) honyaku-translation-buffer-name)
	(setq win-height (+ (* 2 honyaku-window-min) 2))
	(setq lsize (max 0 (- win-height (window-height (selected-window)))))))
      (enlarge-window lsize)
      (setq height (honyaku-get-size-split-rate))
      (split-window (selected-window) height)
      (set-window-buffer (other-window 1) honyaku-candidate-buffer-name)
      (select-window (get-buffer-window honyaku-translation-buffer-name))
      (get-buffer honyaku-candidate-buffer-name))))

(defun honyaku-split-for-dict (origbuf)
  (if (not honyaku-previous-config)
      (progn
	(setq honyaku-previous-config (current-window-configuration))
	(setq honyaku-dict-return-window (selected-window))))
  (let ( (parent-window (selected-window))
	 (height (honyaku-get-size-split-rate)) )
    (cond
     ((get-buffer-window honyaku-dictionary-buffer-name)
      (display-buffer honyaku-dictionary-buffer-name))
     ((string= (buffer-name origbuf) honyaku-candidate-buffer-name)
      (if (< (window-height) 8)
	  (honyaku-warning "ウィンドウが小さ過ぎます。"))
      (split-window (selected-window) height)
      (set-window-buffer (other-window 1) honyaku-dictionary-buffer-name)
      (select-window parent-window))
     ((get-buffer-window honyaku-candidate-buffer-name)
      (set-window-buffer (get-buffer-window honyaku-candidate-buffer-name)
			 honyaku-dictionary-buffer-name))
     ((string= (buffer-name origbuf) honyaku-translation-buffer-name)
      (if (< (window-height) 8)
	  (honyaku-warning "ウィンドウが小さ過ぎます。"))
      (split-window (selected-window) height)
      (set-window-buffer (other-window 1) honyaku-dictionary-buffer-name)
      (select-window parent-window))
     ((get-buffer-window honyaku-translation-buffer-name)
      (set-window-buffer (get-buffer-window honyaku-translation-buffer-name)
			 honyaku-dictionary-buffer-name))
     (t
      (delete-other-windows)
      (if (< (window-height) 8)
	  (honyaku-warning "ウィンドウが小さ過ぎます。"))
      (split-window (selected-window) height)
      (set-window-buffer (other-window 1) honyaku-dictionary-buffer-name)
      (select-window parent-window)))))

(defun honyaku-delete-yaku-window ()
  (let ( (koho-win (get-buffer-window honyaku-candidate-buffer-name))
	 (yaku-win (get-buffer-window honyaku-translation-buffer-name)) )
    (if koho-win
	(if (not (one-window-p))
	    (delete-windows-on (window-buffer koho-win))
	  (switch-to-buffer (other-buffer))))
    (if yaku-win
	(if (not (one-window-p))
	    (delete-windows-on (window-buffer yaku-win))
	  (switch-to-buffer (other-buffer))))
    (if (window-live-p honyaku-yaku-return-window)
	(select-window honyaku-yaku-return-window))
    (if (window-configuration-p honyaku-previous-config)
	(progn
	  (set-window-configuration honyaku-previous-config)
	  (setq honyaku-previous-config nil)))))

(defun honyaku-delete-dict-window ()
  (let ( (dict-win (get-buffer-window honyaku-dictionary-buffer-name)) )
    (if dict-win
	(progn
	  (if (not (one-window-p))
	      (delete-windows-on (window-buffer dict-win))
	    (switch-to-buffer (other-buffer)))
	  (if (window-live-p honyaku-dict-return-window)
	      (select-window honyaku-dict-return-window))))))

(defun honyaku-get-size-split-rate ()
  (1+ (max (/ (* (window-height (selected-window))
	         (- 100 honyaku-split-rate)) 100)
	   honyaku-window-min)))


(defconst EG_PRM_OFFSET_SEED   ?\xF    "識別コード最上位4ビット値")
(defconst EG_INIT              ?\x01   "使用開始")
(defconst EG_END               ?\x02   "使用終了")
(defconst EG_TRANSLATE_ONE     ?\x04   "一文翻訳")
(defconst EG_BROWSE_DICT       ?\x15   "辞書参照")
(defconst EG_GETEQUIV          ?\x07   "訳語対応")
(defconst EG_GETWORD           ?\x08   "訳語候補リストの取得")
(defconst EG_SETWORD           ?\x09   "訳語変更")
(defconst EG_GETHINSHI         ?\x0a   "品詞候補リストの取得")
(defconst EG_SETHINSHI         ?\x0b   "品詞変更")
(defconst EG_GETTRNTXT         ?\x0c   "訳文を返す")
(defconst EG_SETLEARN          ?\x0e   "学習指定")
(defconst EG_SETMODE           ?\x11   "翻訳モードのセット")
(defconst EG_SETDICTNAME       ?\x13   "使用辞書の設定")
(defconst EG_CLEAR_CACHE       ?\x14   "辞書キャッシュのクリア")
(defconst HONYAKU_DIVIDE_SENTENCE ?\x1b   "分割処理")
(defconst EG_SET_TIMEOUT       ?\x1c   "タイムアウトの設定")
(defconst HONYAKU_VERSION         ?\xE001 "プロトコルバージョン")
(defconst HONYAKU_COM_BUF_NAME   "*honyaku-Comm*" "通信プロセスバッファ名")
(defconst HONYAKU_COM_PROC_NAME  "honyaku"        "通信プロセスオブジェクト名")
(defconst HONYAKU_COM_MODE_NAME  "honyaku-Comm"   "通信プロセスバッファモード")

(defun honyaku-com-generate-prm (n)
  (+ (lsh EG_PRM_OFFSET_SEED 20) n))

(defun honyaku-com-start (server port)
  (let ((bufname (generate-new-buffer HONYAKU_COM_BUF_NAME))
	proc err)
    (save-excursion
      (set-buffer bufname)
      (bury-buffer bufname)
      (setq mode-name HONYAKU_COM_MODE_NAME)
      (if (>= emacs-major-version 20)
	  nil
	(setq mc-flag nil))
      (buffer-disable-undo)
      (erase-buffer)
      (setq buffer-read-only t))
    (message "%s" (format "HONYAKU: ホスト %s のポート %d へ接続中・・" server port))
    (condition-case err
	(setq proc
	      (open-network-stream HONYAKU_COM_PROC_NAME bufname server port))
      (error
       (honyaku-output-msg (format "ホスト %s のポート %d への接続: %s" server port
				   err))
       (kill-buffer bufname)
       nil))
    (if proc
	(progn
	  (set-process-sentinel proc 'honyaku-com-sentinel)
	  (if (>= emacs-major-version 20)
	      (progn
		(set-marker-insertion-type (process-mark proc) t)
		(set-process-coding-system proc 'no-conversion 'no-conversion))
	    (set-marker-type (process-mark proc) t)
	    (set-process-coding-system proc '*noconv* '*noconv*))
	  (process-kill-without-query proc)
	  (honyaku-output-msg (format "ホスト %s のポート %d へ接続しました"
				      server port))
	  proc))))

(defun honyaku_eg_init (proc ej_or_je usrnam)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_INIT))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int HONYAKU_VERSION)
      (honyaku-com-set-string usrnam)
      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (honyaku-com-get-int proc)
	(honyaku-com-lock-buffer proc)))))

(defun honyaku_eg_end (proc ej_or_je)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_END))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (setq ret (honyaku-com-get-int proc))
	(honyaku-com-lock-buffer proc)))))

(defun honyaku_eg_translate_one (proc ej_or_je word)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_TRANSLATE_ONE))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int 0)
      (honyaku-com-set-int 4096)
      (honyaku-com-set-short 0)
      (honyaku-com-set-short 0)
      (honyaku-com-set-int 0)
      (honyaku-com-set-euc-string word)
      (honyaku-com-send-data-to-server proc)
      (honyaku-com-get-header proc)
      (setq ret (honyaku-com-get-int proc))
      (if (< ret 0)
	  ret
	(prog2
	    (honyaku-com-get-phrase proc)
	    (honyaku-com-get-euc-string proc)
	  (honyaku-com-lock-buffer proc))))))

(defun honyaku_eg_getequiv (proc ej_or_je which start)
  (let ( (ret) (orignum) (trannum) (numrange) )
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_GETEQUIV))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int which)
      (honyaku-com-set-int start)
      (honyaku-com-send-data-to-server proc)
      (honyaku-com-get-header proc)
      (setq ret (honyaku-com-get-int proc))
      (if (< ret 0)
	  ret
	(prog1
	    (progn
	      (setq orignum (honyaku-com-get-short proc))
	      (setq trannum (honyaku-com-get-short proc))
	      (setq numrange (+ orignum trannum))
	      (setq ret (list ret orignum trannum))
	      (let ((i 0))
		(while (< i numrange)
		  (setq ret
			(nconc ret (list (honyaku-com-get-short proc)
					 (honyaku-com-get-short proc))))
		  (setq i (1+ i))))
	      ret)
	  (honyaku-com-lock-buffer proc))))))

(defun honyaku_eg_getword (proc ej_or_je wordno pbufsiz mode)
  (honyaku_eg_getyakuhin proc ej_or_je wordno pbufsiz mode t EG_GETWORD))
(defun honyaku_eg_gethinshi (proc ej_or_je wordno pbufsiz)
  (honyaku_eg_getyakuhin proc ej_or_je wordno pbufsiz 0 nil EG_GETHINSHI))
(defun honyaku_eg_getyakuhin (proc ej_or_je wordno pbufsiz mode isyakugo eg_get)
  (let ( (ret) (numword) )
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm eg_get))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int wordno)
      (honyaku-com-set-int pbufsiz)
      (if isyakugo (honyaku-com-set-int mode))
      (honyaku-com-send-data-to-server proc)
      (honyaku-com-get-header proc)
      (setq ret (honyaku-com-get-int proc))
      (if (< ret 0)
	  ret
	(prog1
	    (progn
	      (setq numword ret)
	      (honyaku-com-get-int proc)
	      (setq ret (list ret))
	      (let ((i 0))
		(while (< i numword)
		  (setq ret (nconc ret (list (honyaku-com-get-euc-string proc))))
		  (setq i (1+ i))))
	      ret)
	  (honyaku-com-lock-buffer proc))))))

(defun honyaku_eg_setword (proc ej_or_je wordno yakuno)
  (honyaku_eg_setyakuhin proc ej_or_je wordno yakuno t EG_SETWORD))
(defun honyaku_eg_sethinshi (proc ej_or_je wordno yakuno)
  (honyaku_eg_setyakuhin proc ej_or_je wordno yakuno nil EG_SETHINSHI))
(defun honyaku_eg_setyakuhin (proc ej_or_je wordno yakuno isyakugo eg_set)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm eg_set))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int wordno)
      (honyaku-com-set-int yakuno)
      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (honyaku-com-get-int proc)
	(honyaku-com-lock-buffer proc)))))

(defun honyaku_eg_gettrntxt (proc ej_or_je)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_GETTRNTXT))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int 4096)
      (honyaku-com-send-data-to-server proc)
      (honyaku-com-get-header proc)
      (setq ret (honyaku-com-get-int proc))
      (if (< ret 0)
	  ret
	(prog1
	    (honyaku-com-get-euc-string proc)
	  (honyaku-com-lock-buffer proc))))))

(defun honyaku_eg_setlearn (proc ej_or_je learn)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_SETLEARN))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int learn)
      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (honyaku-com-get-int proc)
	(honyaku-com-lock-buffer proc)))))

(defun honyaku_eg_setmode (proc ej_or_je mode)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_SETMODE))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-string mode)
      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (honyaku-com-get-int proc)
	(honyaku-com-lock-buffer proc)))))

(defun honyaku_eg_setdictname (proc ej_or_je use_comp use_learn)
  (let ( (ret) (ndic 1)
	 (val_comp  (if use_comp  1 0))
	 (val_learn (if use_learn 1 0)) )
    (save-excursion
      (if use_comp
	  (setq ndic (1+ ndic)))
      (if use_learn
	  (setq ndic (1+ ndic)))
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_SETDICTNAME))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int val_comp)
      (honyaku-com-set-int val_learn)
      (honyaku-com-set-int ndic)

      (if use_comp
	  (progn
	    (honyaku-com-set-short 3)
	    (honyaku-com-set-padded-bin 2 "CC")
	    (honyaku-com-set-string "computer")))

      (honyaku-com-set-short 0)
      (honyaku-com-set-padded-bin 2 "SS")
      (honyaku-com-set-string "")

      (if use_learn
	  (progn
	    (honyaku-com-set-short 4)
	    (honyaku-com-set-padded-bin 2 "LL")
	    (honyaku-com-set-string (user-login-name))))

      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (honyaku-com-get-int proc)
	(honyaku-com-lock-buffer proc)))))

(defun honyaku_eg_clear_cache (proc ej_or_je)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_CLEAR_CACHE))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (setq ret (honyaku-com-get-int proc))
	(honyaku-com-lock-buffer proc)))))

(defun honyaku_eg_browse_dict (proc ej_or_je word)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_BROWSE_DICT))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int 65536)
      (honyaku-com-set-euc-string word)
      (honyaku-com-send-data-to-server proc)
      (honyaku-com-get-header proc)
      (setq ret (honyaku-com-get-int proc))
      (if (< ret 0)
	  ret
	(prog1
	    (honyaku-com-get-euc-string proc)
	  (honyaku-com-lock-buffer proc))))))

(defun honyaku_divide_sentence (proc ej_or_je string)
  (let ( (ret) (numsen) (sentences) )
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm HONYAKU_DIVIDE_SENTENCE))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int 1)
      (honyaku-com-set-euc-string string)
      (honyaku-com-send-data-to-server proc)
      (honyaku-com-get-header proc)
      (setq ret (honyaku-com-get-int proc))
      (if (< ret 0)
	  ret
	(setq numsen (honyaku-com-get-int proc))
	(setq sentences (honyaku-com-get-euc-string proc))
	(prog1
	    (list ret numsen sentences)
	  (honyaku-com-lock-buffer proc))))))

(defun honyaku_eg_set_timeout (proc ej_or_je lockwaittime tranwaittime)
  (let ((ret))
    (save-excursion
      (honyaku-com-init-buffer proc)
      (honyaku-com-set-int (honyaku-com-generate-prm EG_SET_TIMEOUT))
      (honyaku-com-set-int ej_or_je)
      (honyaku-com-set-int lockwaittime)
      (honyaku-com-set-int tranwaittime)
      (honyaku-com-send-data-to-server proc)
      (prog2
	  (honyaku-com-get-header proc)
	  (honyaku-com-get-int proc)
	(honyaku-com-lock-buffer proc)))))

(defun honyaku-com-sentinel (proc signal)
  (if (string= (process-status proc) "closed")
      (progn
	(kill-buffer (process-buffer proc))
	(setq honyaku-initialized-ej nil)
	(setq honyaku-initialized-je nil)
	(setq honyaku-connected-ej nil)
	(setq honyaku-connected-je nil)
	(honyaku-output-msg "サーバとの接続が切断されました")
	(honyaku-error-log -501))))

(defun honyaku-com-init-buffer (proc)
  (let ((buf (process-buffer proc)))
    (set-buffer buf)
    (bury-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)))

(defun honyaku-com-lock-buffer (proc)
  (set-buffer (process-buffer proc))
  (setq buffer-read-only t))

(defun honyaku-com-set-int (int)
  (insert-char (logand (lsh int -24) 255) 1)
  (insert-char (logand (lsh int -16) 255) 1)
  (insert-char (logand (lsh int -8) 255) 1)
  (insert-char (logand int 255) 1))

(defun honyaku-com-set-short (int)
  (insert-char (logand (lsh int -8) 255) 1)
  (insert-char (logand int 255) 1))

(defun honyaku-com-set-string (str)
  (insert str)
  (insert-char 0 1))

(defun honyaku-com-set-padded-bin (totallength str)
  (let ( (len (length str)) (padlen) )
    (setq padlen (- totallength len))
    (insert str)
    (insert-char ?\x00 padlen)))

(defun honyaku-com-set-euc-string (str)
  (let (euc-str)
    (if (>= emacs-major-version 20)
	(progn
	  (setq euc-str (encode-coding-string str 'euc-jp))
	  (if (multibyte-string-p euc-str)
	      (set-buffer-multibyte t)
	    (set-buffer-multibyte nil))
	  (honyaku-com-set-string euc-str))
      (honyaku-com-set-string (code-convert-string str '*internal* '*euc-japan*)))
    ))

(defun honyaku-com-send-data-to-server (proc)
  (let ( (p) (bsize (buffer-size)) )
    (goto-char (point-min))
    (honyaku-com-set-int ?\xaabbccdd)
    (honyaku-com-set-int bsize)
    (setq p (point-max))
    (process-send-region proc (point-min) (point-max))
    (goto-char p)))

(defun honyaku-com-get-string (proc)
  (let ((p (point)))
    (while (not (search-forward "\0" nil t))
      (honyaku-com-get-data-from-server proc))
    (buffer-substring p (1- (point)))))

(defun honyaku-com-get-euc-string (proc)
  (if (>= emacs-major-version 20)
      (decode-coding-string (honyaku-com-get-string proc) 'euc-jp)
    (code-convert-string (honyaku-com-get-string proc) '*euc-japan* '*internal*)))

(defun honyaku-com-get-phrase (proc)
  (let ((numsh (honyaku-com-get-short proc)))
    (let ((i 0))
      (while (< i numsh)
	(honyaku-com-get-short proc)
	(honyaku-com-get-short proc)
	(honyaku-com-get-short proc)
	(setq i (1+ i))))))

(defun honyaku-com-get-header (proc)
  (let ( (size) (cksum) )
    (setq cksum (honyaku-com-get-int proc))
    (setq size  (honyaku-com-get-int proc))))

(defun honyaku-com-get-int (proc)
  (+ (lsh (honyaku-com-get-1byte proc) 24)
     (lsh (honyaku-com-get-1byte proc) 16)
     (lsh (honyaku-com-get-1byte proc) 8)
     (honyaku-com-get-1byte proc)))

(defun honyaku-com-get-short (proc)
  (+ (lsh (honyaku-com-get-1byte proc) 8)
     (honyaku-com-get-1byte proc)))

(defun honyaku-com-get-1byte (proc)
  (prog1
      (progn
	(if (eobp)
	    (honyaku-com-get-data-from-server proc))
	(following-char))
    (forward-char 1)))

(defun honyaku-com-get-data-from-server (proc)
  (let ((p (point)))
    (accept-process-output proc)
    (goto-char p)))

(provide 'honyaku)
