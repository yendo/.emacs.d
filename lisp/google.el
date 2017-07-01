;; 以下は w3m-search-engine-alist のコピーです
(defvar google-search-engine-alist
  (let ((ja t))
    `(,@(if ja
	    '(("yahoo"
	       "http://search.yahoo.co.jp/bin/search?p=%s"
	       euc-japan)
	      ("yahoo-en"
	       "http://search.yahoo.com/bin/search?p=%s"))
	  '(("yahoo"
	     "http://search.yahoo.com/bin/search?p=%s")
	    ("yahoo-ja"
	     "http://search.yahoo.co.jp/bin/search?p=%s"
	     euc-japan)))
	,@(if ja
	      '(("google"
		 "http://www.google.com/search?q=%s&hl=ja&lr=lang_ja"
		 shift_jis
         "&hl=ja&ie=Shift_JIS&lr=lang_ja")
		("google-en"
		 "http://www.google.com/search?q=%s"))
	    '(("google"
	       "http://www.google.com/search?q=%s")
	      ("google-ja"
	       "http://www.google.com/search?q=%s&hl=ja&lr=lang_ja"
	       shift_jis)))
	("google groups"
	 "http://groups.google.com/groups?q=%s")
	,@(if ja
	      '(("All the Web"
		 "http://www.alltheweb.com/search?web&_sb_lang=ja&cs=euc-jp\
&q=%s"
		 euc-japan)
		("All the Web-en"
		 "http://www.alltheweb.com/search?web&_sb_lang=en&q=%s"))
	    '(("All the Web"
	       "http://www.alltheweb.com/search?web&_sb_lang=en&q=%s")
	      ("All the Web-ja"
	       "http://www.alltheweb.com/search?web&_sb_lang=ja&cs=euc-jp&q=%s"
	       euc-japan)))
	("goo-ja"
	 "http://www.goo.ne.jp/default.asp?MT=%s"
	 euc-japan)
	("excite-ja"
	 "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp\
&lang=jp&tsug=-1&csug=-1&search=%s"
	 shift_jis)
	("lycos-ja"
	 "http://www.lycos.co.jp/cgi-bin/pursuit?query=\"%s\"&cat=jp&\
encoding=shift-jis"
	 shift_jis)
	("altavista"
	 "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search")
	("rpmfind"
	 "http://rpmfind.net/linux/rpm2html/search.php?query=%s"
	 nil)
	("debian-pkg"
	 "http://packages.debian.org/cgi-bin/search_contents.pl\
?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s")
	("debian-bts"
	 "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s")
	("freebsd-users-jp"
	 "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0\
&max=50&format=long&sort=score&dbname=FreeBSD-users-jp"
	 euc-japan)
	("iij-archie"
	 "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s\
&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp\
&hits=95&nice=Nice")
	("waei"
	 "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=1"
	 euc-japan)
	("eiwa"
	 "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=0")
	("kokugo"
	 "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=2"
	 euc-japan)
	("eiei"
	 "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67")
	,@(if ja
		'(("amazon"
		   "http://www.amazon.co.jp/exec/obidos/search-handle-form/\
250-7496892-7797857"
		   shift_jis
		   "url=index=blended&search-type=quick-search&\
field-keywords=%s")
		  ("amazon-en"
		   "http://www.amazon.com/exec/obidos/search-handle-form/\
250-7496892-7797857"
		   nil
		   "url=index=blended&field-keywords=%s"))
	      '(("amazon"
		 "http://www.amazon.com/exec/obidos/search-handle-form/\
250-7496892-7797857"
		 nil
		 "url=index=blended&field-keywords=%s")
		("amazon-ja"
		 "http://www.amazon.co.jp/exec/obidos/search-handle-form/\
250-7496892-7797857"
		 shift_jis
		 "url=index=blended&search-type=quick-search&\
field-keywords=%s"))))))

;;google で検索
;; w3m-url-encode-string の rename 版 (w3m.el を入れてないから)
(defun my-url-encode-string (str &optional coding)
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (cond
             ((eq ch ?\n)               ; newline
              "%0D%0A")
             ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
              (char-to-string ch))      ; printable
             ((char-equal ch ?\x20)     ; space
              "+")
             (t
              (format "%%%02X" ch))))   ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
                  nil))))

;;(require 'browse-url)
;;(require 'thingatpt)
;; google で検索。引数無しだと mini-buffer で編集できる。
(defun google (str &optional flag)
  "google で検索。引数無しだと mini-buffer で編集できる。"
  (interactive
   (list (cond ((or
                 ;; mouse drag の後で呼び出された場合
                 (eq last-command 'mouse-drag-region) ; for emacs
                 (and (eq last-command 'mouse-track) ; for xemacs
                      (boundp 'primary-selection-extent)
                      primary-selection-extent)
                 ;; region が活性
                 (and (boundp 'transient-mark-mode) transient-mark-mode
                      (boundp 'mark-active) mark-active) ; for emacs
                 (and (fboundp 'region-active-p)
                      (region-active-p)) ; for xemacs
                 ;; point と mark を入れ替えた後
                 (eq last-command 'exchange-point-and-mark))
                (buffer-substring-no-properties
                 (region-beginning) (region-end)))
               (t (thing-at-point 'word)))
         current-prefix-arg))
  (let* ((engine
         (if flag
             (let ((completion-ignore-case t))
               (completing-read
                (format "Which engine? (default %s): "
                        "google")
                google-search-engine-alist nil t))
           "google"))
         (info nil) (post-data nil))
         (when (string= engine "")
           (setq engine "google"))

         (setq info (assoc engine google-search-engine-alist))
         (setq post-data
               (cadddr (assoc engine google-search-engine-alist)))

         (setq str (read-from-minibuffer "Search word: " str))
  (browse-url
   (concat
    (format (cadr info) (my-url-encode-string str (caddr info)))
    (when post-data
      (format post-data (my-url-encode-string str (caddr info))))))))
;;google で検索 —— ここまで
