;;-*- Emacs-Lisp -*-


;;; ロードパスの設定

(setq load-path (append (list 
			 "/usr/local/lib/emacs/site-lisp"
;			 "/home/yendo/.emacs.d/iiimecf"
			 "/home/yendo/.emacs.d/lisp" ) load-path ))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(require 'cl)

(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
;    php-mode
;    scala-mode
;    markdown-mode
;    scss-mode
;    haskell-mode
;    google-c-style
;    yaml-mode
;    open-junk-file
    groovy-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))


;; 論理行移動に
(setq line-move-visual nil)


(require 'tramp)
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org mode


;(require 'sr-speedbar)
;(setq sr-speedbar-right-side nil)

;(speedbar-add-supported-extension ".txt")


(setq org-startup-with-inline-images t)
(setq org-startup-folded nil)

(add-hook 'speedbar-mode-hook
          '(lambda()
             (speedbar-add-supported-extension '(".txt" ))))

(setq org-default-notes-file "~/Dropbox/notes.org")
(setq remember-data-file org-default-notes-file)


;; Dropbox がパスに入っているファイルにはバックアップを作らない
(setq backup-enable-predicate
      (lambda (name)
	(not (string-match "Dropbox"
			   name))))
;; フォルダ以下のファイルの自動保存は /tmp に

(setq auto-save-file-name-transforms
      `((".*/Dropbox/.*" ,temporary-file-directory t)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anythng
;(require 'anything-startup)
;(global-set-key (kbd "C-c a") 'anything-migemo)



;(anything-read-string-mode '(string variable command))
;(anything-read-string-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Auto Complete
; http://cx4a.org/software/auto-complete/index.ja.html

;(require 'auto-complete)
;(add-to-list 'ac-dictionary-directories "/usr/share/auto-complete/dict/")
;(require 'auto-complete-config)
;(ac-config-default)


; 補完メニューを自動で表示しない
;(setq ac-auto-show-menu nil)

;2秒後に自動的に表示
;(setq ac-auto-show-menu 0.8)

; クイックヘルプを利用しない
;(setq ac-use-quick-help nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ポップアップウィンドウによる改善
; http://d.hatena.ne.jp/m2ym/20110120/1295524932

;(require 'popwin)
;(setq display-buffer-function 'popwin:display-buffer)


; ウィンドウの簡易切り替え
; http://d.hatena.ne.jp/rubikitch/20100210/emacs

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-z") 'other-window-or-split)

;;通常のウィンドウ用の設定
(setq-default truncate-lines nil)

;;ウィンドウを左右に分割したとき用の設定
(setq-default truncate-partial-width-windows nil)


(setq inhibit-startup-message t) 

; Tramp など一部のファイルを「Recent」メニューに加えたくない
(setq recentf-exclude '("^/[^/:]+:"))


; フォント設定
(cond
 ((= emacs-major-version 24)  ; emacs-23
  (cond (window-system

;        (set-default-font "Migu 2M-10.5")
         (set-face-font 'variable-pitch "Migu 2M")
;         (set-fontset-font (frame-parameter nil 'font)
;                           'japanese-jisx0208
;                           '("IPA Mincho" . "unicode-bmp"))

))))



; tramp

;(setq tramp-default-method "ssh")
;(require 'tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  開発関連

(which-func-mode t)

;; perl プログラム編集用の設定

(setq 
 cperl-close-paren-offset -4
 cperl-indent-level 4
 cperl-continued-statement-offset 4
 cperl-comment-column 40
 cperl-indent-parens-as-block t
 cperl-tab-always-indent t
 cperl-highlight-variables-indiscriminately t
 )


(defun perltidy-region ()               ;選択regionをperltidy
   "Run perltidy on the current region."
   (interactive)
   (save-excursion
     (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()                ;開いているソースをperltidy
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))




; changelog mode
(setq change-log-default-name "changelog")
(setq add-log-full-name "Yoshizumi Endo")

;; インデントにタブを利用しない
(setq-default indent-tabs-mode nil)

;;; 外観の設定

(set-face-background 'cursor "red" nil)
(tool-bar-mode 0)

(add-hook 'input-method-activate-hook
          (function (lambda () (set-cursor-color "red"))))
(add-hook 'input-method-inactivate-hook
          (function (lambda () (set-cursor-color "blue"))))

; 日本語 grep
;(if (file-exists-p "/usr/bin/lgrep")
;    (setq grep-command "lgrep -n -r ")
;)

(if window-system
    (progn 
      (global-set-key [delete] 'delete-char)
      (setq x-select-enable-clipboard t) ; emacs -> clipboad
;      (set-face-background 'tool-bar   "#efebe7")
;      (set-face-background 'scroll-bar "#efebe7")
;      (setq browse-url-browser-function 'browse-url-netscape)
;      (setq browse-url-netscape-program "mozilla")
;      (let ((mpointer (x-get-resource "*mpointer" "*emacs*mpointer")))
;	(if (eq mpointer nil)
;	    (setq mpointer "60")) ; hand2 (60)
;	(setq x-sensitive-text-pointer-shape (string-to-int mpointer)))
      (set-face-background 'mouse      "white")
      (set-scroll-bar-mode 'right)
  (setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "iceweasel")

  )

  (progn 
    (menu-bar-mode 0)
    (display-time)

    (setq browse-url-browser-function 'w3m-browse-url)
    (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;    (define-key function-key-map [backspace] [8])
;    (put 'backspace 'ascii-character 8)
;    (keyboard-translate ?\C-h ?\C-?)
;    (set-face-background 'modeline "white")
;    (set-face-foreground 'modeline "blue")


     (if (equal (getenv "TERM") "xterm-256color")
 	(progn
 	  (global-set-key "\M-h" 'help-for-help)
	  (global-set-key [select] 'end-of-buffer)

;	  (set-face-inverse-video-p 'modeline t)
;	  (set-face-inverse-video-p 'modeline-inactive t)
;	  (set-face-background 'modeline-inactive "white")
;	  (set-face-foreground 'modeline-inactive "brightblack")
; 	  (elscreen-set-prefix-kye "\C-t")
; 	  (set-face-background 'modeline "blue")
; 	  (set-face-foreground 'modeline "white")
 	  ))
     ))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ライブラリのロード


(require 'grep-edit)
(require 'ahg)
(require 'quickrun)


;;zlc.el で Emacs のミニバッファ補完を zsh ライクに
;(require 'zlc)


(when (equal system-name "bob3.tsurukawa.org")

  (add-to-list 'load-path "/usr/share/emacs24/site-lisp/emacs-mozc")
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)

  (add-hook 'mozc-mode-hook
            (lambda()
              (define-key mozc-mode-map (kbd "<zenkaku-hankaku>") 'toggle-input-method)))
  )

;; マシン毎に設定を切り替える

;(load "migemo")

(if (equal (system-name) "bob3")
    (progn

      ; C/Migemo
;      (load "migemo")
;      (setq migemo-command "cmigemo")
;      (setq migemo-options '("-q" "--emacs" "-i" "\a"))
;      (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")


      (load-library "tails-history.el")
;      (gnuserv-start)
))

(require 'server)
(unless (server-running-p)
  (server-start))



;;; ロード

(autoload 'pmemo "pmemo" nil t)
(autoload 'google "google" nil t)
(autoload 'lookup-pattern "lookup" nil t)

(load "generic-x")
(load "custom")
(load "recent-dired")

(load "text-adjust")
(setq text-adjust-rule-kutouten text-adjust-rule-kutouten-zkuten)

;(load "midnight")
;(setq midnight-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fly spell

;(dolist (hook '(text-mode-hook))
;  (add-hook hook (lambda () (flyspell-mode 1))))

;(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;  (add-hook hook (lambda () (flyspell-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 翻訳

;; require でも autoload でもおすきな方をどうぞ.
;; 最近だと, require の方が推奨されているとのことです.

;;(autoload 'text-translator "text-translator" "Text Translator" t)
(require 'text-translator)

;; プリフィックスキーを変更する場合.
;; (setq text-translator-prefix-key "\M-n")

(setq text-translator-default-engine "excite.co.jp_enja")

(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)
;; グローバルキーを設定
(global-set-key "\C-xt" 'text-translator-translate-by-auto-selection)
(global-set-key "\C-x\M-t" 'text-translator)
(global-set-key "\C-x\M-T" 'text-translator-translate-last-string)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 補完

;;; zsh 風の補完

(load "minibuffer-complete-cycle")
(setq minibuffer-complete-cycle t)

;;; M-! の補完

(require 'shell-command)
(shell-command-completion-mode)

;; 補完しないファイル名サフィックス

(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      '(".exe" ".aux" ".xls" ".doc")))

;; 部分一致補完
; http://www.gentei.org/~yuuji/rec/pc/memo/2006/03/19/
; (partial-completion-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他

; 個人用 info 

(setq Info-default-directory-list
      (append Info-default-directory-list 
	      (list (expand-file-name "~/Documents/info/"))))

; 質問に y or n で答える

(fset 'yes-or-no-p 'y-or-n-p)

; メニュー項目消去

(define-key menu-bar-tools-menu [rmail] nil)
(define-key (lookup-key global-map [menu-bar]) [elscreen] nil)

; Window の切り替え

(windmove-default-keybindings)

; 同名のバッファを区別

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;; mouse で paste する際、mouse の位置ではなく cursor の位置に paste する。
(setq mouse-yank-at-point t)

(setq diff-switches "-u")

(setq transient-mark-mode nil)

(setq ange-ftp-ftp-program-args '("-i" "-n" "-g" "-v" "-p"))

; スクリプトファイル保存時に自動で実行許可フラグを立てる

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; 海馬日記 - CopyURL+とEmacsに關するメモ
;; http://d.hatena.ne.jp/mhrs/20050314/p1
;; 通常の X のペーストができなくなる
;; (setq x-select-enable-clipboard nil)

(recentf-mode)


(set-default 'line-spacing 0.2)

;(if (= emacs-major-version 23)
;       (set-default 'line-spacing 0.2)
;       (set-default 'line-spacing 1)
;)


(setq pgg-default-scheme 'gpg)
(setq pgg-scheme 'gpg)

(setq gnus-directory "~/.News/"
      message-directory "~/.News"
      message-auto-save-directory "~/.News/drafts/")


; draft?
(setq message-draft-coding-system 'iso-2022-7bit)

; memo
;(defun memo ()
;  (interactive)
;  (let ((add-log-current-defun-function 'ignore)
;        (memo-file "~/text/memo.txt"))
;    (switch-to-buffer (find-file-noselect memo-file))
;    (add-change-log-entry
;     nil
;     (expand-file-name memo-file))))
;(define-key ctl-x-map "M" 'memo)


(defun memo ()
  (interactive)
  (let ((add-log-current-defun-function 'ignore)
        (memo-file "~/Dropbox/memo.txt")
        (date (format-time-string "%Y-%m-%d"))
        (time (format-time-string "%H:%M"))
        )
    (switch-to-buffer (find-file-noselect memo-file))
    (org-mode)
    (beginning-of-buffer)

    (unless (looking-at (concat "^* " date))
      (insert "* " date "\n\n")
      (beginning-of-buffer)
      )

    (next-line)
    (insert "\n** " time "\n")
    (backward-char (+ 1 (length time)))
    ))
(define-key ctl-x-map "M" 'memo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; message 関連

;(setq send-mail-function 'smtpmail-send-it)
;(setq smtp-server "master")
;(setq smtp-local-domain "tsurukawa.org")

(setq smtpmail-smtp-server "master")
(setq smtpmail-local-domain "tsurukawa.org")
(setq message-send-mail-function 'message-smtpmail-send-it)
(setq message-user-fqdn "tsurukawa.org")


;(setq message-mime-send-mail-function 'message-send-mail-with-smtp)
;(setq smtp-default-server "master")

; active でない window の空 cursor を出さない
(setq cursor-in-non-selected-windows nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp-mode

(define-key emacs-lisp-mode-map "\C-c;" 'comment-region)
(define-key emacs-lisp-mode-map "\C-c:" 'uncomment-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode

;(setq c-mode-hook '(lambda()
;		     (setq c-default-style "k&r")
;		     (global-set-key "\C-m" 'newline-and-indent)
;		     (setq c-basic-offset 4)
;		     (define-key c-mode-base-map "\C-c\C-c" 'recompile)
;		     (define-key c-mode-base-map "\C-cc" 'comment-region)
;		    (setq c-tab-always-indent t)
;		    (setq indent-tabs-mode nil)
;		     ))
;(define-key c-mode-base-map "\C-cc" 'compile)

(setq latex-mode-hook '(lambda()
		     (local-unset-key "\C-m \C-w")
))

(setq w3m-mode-hook '(lambda()
		     (local-unset-key "\C-m \C-w")
))

;(make-local-hook 'html-mode-hook)

;(setq auto-mode-alist
;      (cons '("\\.rcp$" . pilrc-mode) auto-mode-alist))
;(setq interpreter-mode-alist
;      (cons '("pilrc" . pilrc-mode)
;	    interpreter-mode-alist))
;(autoload 'pilrc-mode "pilrc-mode" "PilRC editing mode." t)



(setq honyaku-server-name "master")

;(setenv "LANG" "C")
;(setenv "LC_TIME" "c")
(setenv "PERL_BADLANG" "0")

(blink-cursor-mode 0)


; 印刷関係

(setenv "RLPR_PRINTHOST" "master")
;(setq ps-paper-type 'a4)
;(setq lpr-command "rlpr")

(setq ps-multibyte-buffer 'non-latin-printer)
(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 言語設定

(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)

;(setq default-buffer-file-coding-system 'euc-jp)

;; 曖昧な二バイト文字の問題 (atok invalid pos)

(if (< emacs-major-version 23)
  (utf-translate-cjk-set-unicode-range
   '((#x00a2 . #x00a3) ; C|, L-
     (#x00a7 . #x00a8) ; §, ¨
     (#x00ac . #x00ac) ; ¬
     (#x00b0 . #x00b1) ; °, ±
     (#x00b4 . #x00b4) ; ´
     (#x00b6 . #x00b6) ; ¶
     (#x00d7 . #x00d7) ; ×
     (#X00f7 . #x00f7) ; ÷
     (#x0370 . #x03ff) ; Greek and Coptic
     (#x0400 . #x04FF) ; Cyrillic
     (#x2000 . #x206F) ; General Punctuation
     (#x2100 . #x214F) ; Letterlike Symbols
     (#x2190 . #x21FF) ; Arrows
     (#x2200 . #x22FF) ; Mathematical Operators
     (#x2300 . #x23FF) ; Miscellaneous Technical
     (#x2500 . #x257F) ; Box Drawing
     (#x25A0 . #x25FF) ; Geometric Shapes
     (#x2600 . #x26FF) ; Miscellaneous Symbols
     (#x2e80 . #xd7a3) 
     (#xff00 . #xffef))
   )
  )

(autoload 'ac-mode "ac-mode" "Minor mode for advanced completion." t nil)
(require 'shell-command)
(require 'minibuf-isearch)

;; セッション

(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  ;; これがないと file-name-history に500個保存する前に max-string に達する
  (setq session-globals-max-string 100000000)
  ;; デフォルトでは30!
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize))

(setq ac-mode-goto-end-of-word t)
(setq gc-cons-threshold 1000000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;; 全般の設定

(setq scroll-step 1)
(setq next-line-add-newlines nil)
(setq kill-whole-line t)
(setq fill-column 74)
(setq ange-ftp-generate-anonymous-password "y-endo@ceres.dti.ne.jp")
(substitute-key-definition 'rmail 'gnus-no-server menu-bar-tools-menu)
(setq font-lock-support-mode 'jit-lock-mode)
;(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
;(setq lazy-lock-defer-time nil)
(line-number-mode t)

;(resize-minibuffer-mode 0)
(auto-compression-mode 1)
;(setq x-face-mule-force-save-cache-file t)

;(autoload 'namazu "namazu" nil t)


;; festival
;(autoload 'say-minor-mode "festival" "Menu for using Festival." t)
;(say-minor-mode t)

;; man 関連

;'r'キーで関連項目の man ページにジャンプしたい。
(setq Man-see-also-regexp "SEE ALSO\\|関連項目")

;各ヘッダ間を'n','p'でジャンプする。
(setq Man-first-heading-regexp
            "^[ \t]*NAME$\\|^[ \t]*名[前称]$\\|^[ \t]*No manual entry fo.*$")
      (setq Man-heading-regexp "^\\([A-Zーぁ-んァ-ヶ亜-瑤][A-Zーぁ-んァ-ヶ亜-瑤 \t]+\\)$")


;; レジスタ
(set-register ?m '(file . "/home/yendo/memo.txt"))
(set-register ?e '(file . "/home/yendo/.emacs"))
(set-register ?d '(file . "/home/yendo/text/doctor/c5.tex"))

;(set-register ?t '(file . "/home/yendo/development/google_work/twitim/twitim"))
(set-register ?t '(file . "/home/yendo/development/twitim/twitim/twitim"))
(set-register ?g '(file . "~/development/hg/gphotoframe/"))
(set-register ?f '(file . "~/development/hg/gfeedline/"))
(set-register ?i '(file . "~/development/isac/"))


(setq display-time-string-forms
      '(" " month "/" day " " dayname " " 
	      24-hours ":" minutes))

;;; フレームタイトル

(setq frame-title-format "%b")
(setq icon-title-format  "%f")

;; windows.el

; (setq win:frame-title-function nil) ; フレームタイトルを変更しない
; (setq win:use-frame nil) ; 新規にフレームを作らない

;; elscreen

(if window-system
    (setq elscreen-display-tab nil)
  (setq elscreen-display-tab nil))

(setq elscreen-tab-display-create-screen nil
      elscreen-tab-display-kill-screen nil
      elscreen-tab-width 14)

;(define-key elscreen-map "\C-z" 'elscreen-toggle)
;(define-key elscreen-map (kbd "SPC") 'elscreen-next)
;(define-key elscreen-map (kbd "C-SPC") 'elscreen-next)
;(define-key elscreen-map "\C-@" 'elscreen-next)


(global-set-key [S-f2] 'swap-screen) ; 画面上下の切替
(global-set-key [f2] 'swap-screen-with-cursor) ; 画面上下の切替

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド

;(autoload 'browse-yank "browse-yank" nil t)        ; browse-yank.el


(global-set-key [zenkaku-hankaku] 'toggle-input-method)
(global-set-key "\e\C-y" 'clipboard-yank)
(define-key global-map [f1] 'buffer-menu)
;(define-key global-map [f2] 'other-window)
(define-key global-map [f3] 'vzlike-change-buffer)
(define-key global-map [f4] 'vzlike-split-window)
(define-key global-map [f7] 'query-replace)
;(define-key global-map [f9] 'canna-kigou-mode)
(define-key global-map [f9] 'special-symbol-input)
;(define-key global-map [f10] 'canna-extend-mode)
(define-key global-map [S-f10] 'find-file-at-point)
;(define-key global-map [f11] 'canna-bushu-mode)
;(define-key global-map [f12] 'canna-touroku-region)
(define-key global-map [f12] 'egg-toroku-region)
(global-set-key "\M-?" 'comint-dynamic-complete)
;(global-set-key "\M-?" 'hippie-expand)
;(global-set-key "\C-xb" 'iswitchb-buffer)
(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'backward-word)
;(global-set-key [S-right] 'forward-word)
;(global-set-key [S-left] 'backward-word)
;(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-xn" 'gnus)
;(global-set-key "\C-c\C-d" 'online-dictionary)
;(global-set-key "\C-ho" 'lookup-pattern-other-frame)
(global-set-key "\C-ho" 'lookup-pattern)
(global-set-key "\C-xm" 'message-mail)
(global-set-key "\C-xp" 'twit-window-post)
;(global-set-key "\C-cr" 'gnus-no-server)
(global-unset-key "\C-xf")
(global-set-key "\C-xf" 'recentf-open-files)
(global-set-key [S-mouse-2] 'browse-url-at-mouse)

(xterm-mouse-mode 1)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)


(global-set-key "\C-hg" 'google)
(global-set-key [end]  'end-of-buffer )
(global-set-key [home] 'beginning-of-buffer )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; バッファ関連

(iswitchb-mode)
;(iswitchb-default-keybindings)

(define-key ctl-x-map  "\C-b" 'buffer-menu) ; buffer list 表示後カーソルを移動
;(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-x\C-b" 'ibuffer)


;iswitchb で選択中の内容を表示
;http://www.bookshelf.jp/soft/meadow_28.html#SEC371

(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "選択している buffer を window に表示してみる。"
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 各種モード

;;; モードリスト

(setq auto-mode-alist
      (append
       '(("mailrc"    . mailrc-generic-mode)
         (".SCORE"  . emacs-lisp-mode)
	 (".wml" . html-mode)
         (".procmailrc"  . procmail-mode))
       auto-mode-alist))

;(autoload 'x-resource-generic-mode "generic-x" nil t)
;(autoload 'mailrc-generic-mode "generic-x" nil t)



;;; debian-www mode

;(define-key global-map "\C-c\C-l" 'debian-www-checklist)
;(autoload 'debian-www-checklist "debwww" nil t)
;(autoload 'debian-www-mode "debwww" nil t)
;(setq html-mode-hook '(lambda () (debian-www-mode 1)))
;(autoload 'base-query "msquery" nil t)


;; View Mode
; view-mode ではモードラインのマイナーモード名に色付けする 
; http://homepage1.nifty.com/blankspace/emacs/view.html

(eval-after-load "view"
  '(setcar (cdr (assq 'view-mode minor-mode-alist))
           (if (fboundp 'propertize)
               (list (propertize " View" 'face 
				 (if window-system '(:foreground "red")
;				    '(:background "blue"))))
				    '(:foreground "red"))))
             " View" )))

(add-hook 'view-mode-hook
          '(lambda ()
             (define-key view-mode-map " " 'scroll-up)
             (define-key view-mode-map [backspace] 'scroll-down) ))


;;; text mode

(add-hook 'text-mode-hook
	  '(lambda ()
	     (progn
	       (set-fill-column 74)
	       (turn-on-auto-fill)
	       (outline-minor-mode t)
	       )))

;;; TeX mode

(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-LaTeX-command-default "pLaTeX")
(setq japanese-LaTeX-default-style "jarticle")
(setq TeX-printer-list nil)
(setq TeX-print-command "rlpr -d %d")
;(setq TeX-print-command "dviprt %d | lpr")
;(setq TeX-print-command "dvi2ps %d | lpr -Prlp")

(setq-default TeX-master nil)
(setq TeX-parse-self nil)
(setq TeX-auto-save nil)

(eval-after-load "tex"
  '(setq TeX-command-list
         (append TeX-command-list
		 (list
		  (list "DVIPDFMX" "dvipdfmx %d" 'TeX-run-command t nil)
		  (list "ACROREAD" "acroread %s.pdf" 'TeX-run-command t nil)
		  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired

(defface my-face-f-2 '((t (:foreground "red"))) nil)
(defvar my-face-f-2 'my-face-f-2)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (concat (format-time-string "%Y-%m-%d" (current-time)) " [0-9]....") arg t))

(add-hook 'dired-mode-hook
          (lambda ()
            (load "dired-x")
	    (load-library "ls-lisp")
            (setq dired-omit-files-p t)
	    (setq ls-lisp-dirs-first t)
	    (font-lock-add-keywords 
	     major-mode
	     (list '(my-dired-today-search . my-face-f-2)))
	    (setq dired-listing-switches "-lBLh")
	    (setq dired-omit-files
		  (concat dired-omit-files "\\|^\\..+$"))
            (setq dired-omit-extensions (append '(".out" ".dvi")
                                                dired-omit-extensions))
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; メッセージモード

(setq user-mail-address "yendo0206@gmail.com"
      ; user-mail-address "yendo@tsurukawa.org"
      message-from-style 'angles
      )

(setq message-generate-headers-first t)
(setq message-default-mail-headers "Bcc: yendo@tsurukawa.org")

(add-hook 'message-mode-hook
	  '(lambda ()
	     (set-buffer-file-coding-system 'junet)
	     (set-fill-column 70)
	     (turn-on-auto-fill)))

(setq message-cite-function 'message-cite-original-without-signature)
(setq message-use-multi-frames t)
(setq message-delete-frame-on-exit t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 略称展開

(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq dabbrev-case-replace nil)
;(setq save-abbrevs t)
;(quietly-read-abbrev-file)



;; http://d.hatena.ne.jp/itouhiro/20091122
;(defadvice dabbrev-expand (around jword (arg) activate)
;   (interactive "*P")
;   (let* ((regexp dabbrev-abbrev-char-regexp)
;          (dabbrev-abbrev-char-regexp regexp)
;          char ch)
;     (if (bobp)
;         ()
;       (setq char (char-before)
;             ch (char-to-string char))
;       (cond
;        ;; ァ〜ヶの文字にマッチしてる時はァ〜ヶが単語構成文字とする
;        ((string-match "[ァ-ヶー]" ch)
;         (setq dabbrev-abbrev-char-regexp "[ァ-ヶー]"))
;        ((string-match "[ぁ-んー]" ch)
;         (setq dabbrev-abbrev-char-regexp "[ぁ-んー]"))
;        ((string-match "[亜-瑤]" ch)
;         (setq dabbrev-abbrev-char-regexp "[亜-瑤]"))
;        ;; 英数字にマッチしたら英数字とハイフン(-)を単語構成文字とする
;        ((string-match "[A-Za-z0-9]" ch)
;;;         (setq dabbrev-abbrev-char-regexp "[A-Za-z0-9]"))
;         (setq dabbrev-abbrev-char-regexp "[A-Za-z0-9-]")) ; modified by peccu
;        ((eq (char-charset char) 'japanese-jisx0208)
;         (setq dabbrev-abbrev-char-regexp
;               (concat "["
;                       (char-to-string (make-char 'japanese-jisx0208 48 33))
;                       "-"
;                       (char-to-string (make-char 'japanese-jisx0208 126 126))
;                       "]")))))
;     ad-do-it))
;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ispell

(setq-default ispell-program-name "aspell")
;(setq ispell-dictionary-alist
;      (cons '("spanish"
;	      "[a-zA-Z\"\\]" "[^a-zA-Z\"]" "[~\']" t ("-d" "spanish")
;	       "~TeX")
;	    ispell-dictionary-alist))

;; 日本語混じりの文章に ispell 
;; Message-ID: <yosuvggf1elh.fsf@jpl.org>

(eval-after-load "ispell"
 '(setq ispell-skip-region-alist (cons '("[^\000-\377]+")
					ispell-skip-region-alist)))

;; latex 文書を扱う
(setq ispell-filter-hook-args '("-w"))
(setq TeX-mode-hook
      (function
       (lambda ()
         (setq ispell-filter-hook "detex"))))

;(ispell-change-dictionary "american")

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;; bbdb
; 
; (require 'bbdb)
; (require 'bbdb-gnus)
; (require 'bbdb-hooks)
; ;(bbdb-initialize 'gnus 'messeage)
; (bbdb-initialize 'gnus)
; 
; (setq bbdb-use-pop-up nil
;       bbdb-offer-save 'no-ask)
; (setq mail-user-agent 'gnus-user-agent)
; (setq bbdb-complete-name-allow-cycling t)

; mail-alias の展開

(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'gnus-message-setup-hook 'bbdb-define-all-aliases)

(defun message-make-from ()
  "Make a From header."
  (with-temp-buffer
    (mm-enable-multibyte)
    (insert (user-full-name)" <" (message-make-address) ">")
    (buffer-string)))

;;; End of the File

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anything-c-use-standard-keys t)
 '(bbdb-file-coding-system (quote utf-8))
 '(blink-cursor-mode nil)
 '(default-input-method "japanese-mozc")
 '(japanese-LaTeX-style-list
   (quote
    (("j-article")
     ("j-report")
     ("j-book")
     ("jslides")
     ("jarticle")
     ("jreport")
     ("jbook")
     ("tarticle")
     ("treport")
     ("tbook")
     ("jsarticle")
     ("jsbook")
     ("abook"))))
 '(lpr-command "lp")
 '(mm-inline-large-images t)
 '(mm-inline-text-html-with-images t)
 '(safe-local-variable-values
   (quote
    ((test-case-name . twisted\.test\.test_policies)
     (test-case-name . twisted\.protocols\.test\.test_basic)
     (test-case-name . twisted\.web\.test\.test_webclient)
     (test-case-name . twittytwister\.test\.test_streaming)
     (test-case-name . twisted\.internet\.test\.test_gtk3reactor)
     (test-case-name . twisted\.internet\.test)
     (TeX-master . t)
     (TeX-command-default . "pLaTeX"))))
 '(session-use-package t nil (session))
 '(w3m-home-page "/home/yendo/.w3m/bookmark.html")
 '(w3m-key-binding (quote info))
 '(w3m-search-default-engine "google-ja")
 '(w3m-use-header-line nil)
 '(wnn-jserver (quote ("master"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
