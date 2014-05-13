;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time-stamp: <2014-05-13 16:55:21 seto>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド
;;; === YaTeX ===
;;; command + T, command + B       : タイプセット (C-c C-t j)
;;; shift + command + B            : タイプセット with bibtex  (C-c C-t b)
;;; shift + command + I            : タイプセット with makeindex (C-c C-t i)
;;; shift + command + P            : プレビュー (C-c C-t p)
;;; shift + command R, C-c s       : Skim PDF カーソル位置表示
;;; ctrl + command + J             : 対応オブジェクトへのジャンプ (C-c C-g)
;;; shift + option + command + B   : BibDesk を開く
;;; shift + option + command + P   ; Preview.app で PDF ファイルを開く
;;; shift + command + H            : 入力ファイルを表示 YaTeX-display-hierarchy (C-c C-d)
;;; command + 1                    : メインファイルのバッファを開く (C-c ^)
;;; command + 2                    : 1つ前のバッファを開く
;;; Tab                            : インデント (latex-indent)
;;; C-c Tab                        : 領域をインデント (latex-indent)
;;; C-c d                          : latexmk -c を実行
;;; command + "_"                  : "_{\mathrm{}}" を挿入
;;; === グローバル ===
;;; C-c w                          : OSX の辞書で調べる
;;; C-c g                          : 選択したテキストをGoogleで検索
;;; C-c k                          : ファイル名の補完
;;; C-;                            : スペルチェック
;;; shift + command + O            : Finder に表示
;;; option + shift + command + F   : Finder でフォルダを開く
;;; option + shift + command + T   : Terminal/iTerm でフォルダを開く
;;; option + command + H           : 実行されているその他すべてのアプリケーションのウインドウを隠す
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons (expand-file-name "~/Dropbox/emacs/yatex") load-path))

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))

;;; インデント (YaTeXのインデントを使わない）
;;; http://www.hit.ac.jp/~wachi/misc/latexindent.html
(autoload 'latex-indent-command "~/Dropbox/emacs/latex-indent"
  "Indent current line accroding to LaTeX block structure.")
(autoload 'latex-indent-region-command "~/Dropbox/emacs/latex-indent"
  "Indent each line in the region according to LaTeX block structure.")

;;; シェルスクリプト SemiAutoTeX でタイプセット
(setq tex-command "latexmk")
;;(setq tex-command "/usr/local/texlive/2013/bin/x86_64-darwin/latexmk")
(setq bibtex-command "semiautotex.sh -b"
      makeindex-command "semiautotex.sh -i"
      YaTeX-typeset-auto-rerun nil ; rerun 機能を無効
      dvi2-command "open -a Skim" ; PDF プレビュアとして Skim.app を使う
      )

;;; YaTeX カスタマイズ変数
(setq YaTeX-inhibit-prefix-letter t ; YaTeXキーバインドを C-c ? から C-c C-? に変更
      YaTeX-kanji-code nil ; 文字コードを変更しない
      YaTeX-use-AMS-LaTeX t ; amsmath を利用
      YaTeX-default-pop-window-height 7 ; タイプセットの時のウィンドウの高さ
      YaTeX-skip-default-reader  t ; 補完入力でミニバッファから入力しない
      YaTeX-latex-message-code 'utf-8
      YaTeX-template-file "~/Dropbox/emacs_and_tex/template.tex" ; 新規作成時のテンプレート
      YaTeX::ref-labeling-section-level 4 ; ref 補完で subsubsection までリストアップ
      )

;;; begin型「C-c C-b 1文字」補完のカスタマイズ
(setq yatex-mode-load-hook
      '(lambda() 
         (YaTeX-define-begend-key "be" "equation") ; デフォルトは enumerate
         (YaTeX-define-begend-key "bE" "enumerate") ; デフォルトは equation
         (YaTeX-define-begend-key "ba" "align")
         (YaTeX-define-begend-key "bg" "gather")
         (YaTeX-define-begend-key "bf" "figure")
         ))

;;;; Skim PDF カーソル位置表示
;;; Emacs から Skim へ
;;; この機能を使うためには、
;;; pdflatex/platex にオプション -synctex=1 が必要
;;;
;;; Thanks to Tsuchiya-san's corrections (yatex ML [yatex:04810,04811])
(defun skim-forward-search ()
  (interactive)
  (process-kill-without-query
   (start-process  
    "displayline"
    nil
    "/Applications/Skim.app/Contents/SharedSupport/displayline"
    (number-to-string (save-restriction
                        (widen)
                        (count-lines (point-min) (point))))
    (expand-file-name
     (concat (file-name-sans-extension (or YaTeX-parent-file
                                           (save-excursion
                                             (YaTeX-visit-main t)
                                             buffer-file-name)))
             ".pdf"))
    buffer-file-name)))

;;;; Preview.app で開く
(defun MyTeX-open-PreviewApp ()
  (interactive)
  (let* ((mtf)
         (pf)
         (cmd "open -a Preview.app"))
    (setq mtf YaTeX-parent-file)
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (process-kill-without-query
     (start-process-shell-command "Open Preview.app" nil cmd pf))))

;;; latexmk -c を実行（TeX の不用なファイルを削除）
(defun MyTeX-latexmk-cleanup ()
  (interactive)
  (let* ((yn))
    (if (y-or-n-p "cleaning up all nonessential files?")
	(progn
	  (message nil)
	  (process-query-on-exit-flag 
	   (start-process-shell-command "latexmk clean-up" nil "latexmk -c"))
	  (message "latexmk clean-up done"))
      )))

;;; ローマン体の下付き添え字
(defun MyTeX-insert-subscript_rm ()
  (interactive)
  (insert "_{\\mathrm{}}")
  (backward-char 2))

;;; 1つ前の buffer に切り替える
(defun MyTeX-switch-to-previousbuffer ()
  (interactive)
  (switch-to-buffer nil))

;;; say コマンドで文を読み上げる
(defun MyTool-speech (b e)
  "Convert text to audible speech by /usr/bin/say of OSX."
  (interactive "r")
  (let (str)
    (if (eq mark-active 'nil)
	(progn (setq b1 (save-excursion (re-search-backward "^\\ *%")
					(point)))
	       (setq b2 (save-excursion (re-search-backward "^\n")
					(point)))
	       (setq e1 (save-excursion (re-search-forward "^\\ *%")
					(point)))
	       (setq e2 (save-excursion (re-search-forward "^\n")
					(point)))
	       (setq b (if (< b1 b2) b2 t b1))
	       (setq e (if (> e1 e2) e2 t e1))))
    (setq str (buffer-substring-no-properties b e))
    (setq str (replace-regexp-in-string "\\\\%" "percent" str))
    (setq str (replace-regexp-in-string "%[^\n]*" "" str))
    (setq str (replace-regexp-in-string "\\\\[a-zA-Z]+{" " " str))
    (setq str (replace-regexp-in-string "[$_^\n()~{}|;`]" " " str))
    (setq str (replace-regexp-in-string "\\ +" " " str))
    (message str)
    (if (eq (process-status "speech") 'run)
	(delete-process "speech")
      (process-kill-without-query
       (start-process-shell-command "speech" nil 
				    "/usr/bin/say -r 220" (concat "\"" str "\"" ))))))


;;; ファイル名の補完(広瀬さん)
(defun MyTool-file-complete ()
  (interactive)
  (let*((p (point))
	(s (save-excursion
	     (skip-chars-backward "^ \t\n:;\"\'()<>{}")
	     (point)))
	(path (buffer-substring s p))
	(dir (or (file-name-directory path) ""))
	(file (file-name-nondirectory path))
	(res (file-name-completion file dir)))
    (cond
     ((eq res t)	(message "Sole completion"))
     ((eq res nil)	(ding) (message "No match!"))
     ((string= file res)
      (message (mapconcat 'princ (file-name-all-completions file dir) " ")))
     (t
      (delete-region p s)
      (insert dir res)))))

;;; 選択範囲を google で検索
(defun MyTool-search-google()
  "Search by google"
  (interactive)
  (let* ((str (string-word-or-region)))
    (browse-url
     (concat "http://google.com/search?q=\"" str "\""))))

;;; 選択範囲を google scholar で検索
(defun MyTool-search-googlescholar()
  "Search string by google scholar"
  (interactive)
  (let* ((str (string-word-or-region)))
    (browse-url
     (concat "http://scholar.google.com/scholar?q=\"" str "\""))))
  
;;; OSX の辞書で調べる(Sakito さん)
;;; http://sakito.jp/mac/dictionary.html
(defun MyTool-lookup-dictionary-osx()
  "Look up the word by Dictionary.app of Mac OS X"
  (interactive)
  (let* ((str (url-hexify-string (string-word-or-region))))
    (browse-url (concat "dict://" str ))))

;;; 単語の発音
(defun MyTool-speech-word()
  "Speak words."
  (interactive)
  (let* ((str (url-hexify-string (string-word-or-region))))
    (process-kill-without-query
     (start-process-shell-command "speech" nil 
				  "/usr/bin/say -r 150" (concat "\"" str "\"" )))))

(defun string-word-or-region ()
  "If a region is selected, the text string of the region is returned.
  Otherwise, the text string of the word is returnd."
  (let ((editable (not buffer-read-only))
        (pt (save-excursion (mouse-set-point last-nonmenu-event)))
        beg end)
    (if (and mark-active
             (<= (region-beginning) pt) (<= pt (region-end)) )
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char pt)
        (backward-char 1)
        (setq end (progn (forward-word) (point)))
        (setq beg (progn (backward-word) (point)))))
    (buffer-substring-no-properties beg end)))

;;; BibDesk で開く
(defun MyTeX-open-item-BibDesk ()
  "Open Bibdesk with the cite key"
  (interactive)
  (let* ((s (save-excursion
	      (skip-chars-backward "^{,")
	      (point)))
	 (e (save-excursion
	      (skip-chars-forward "^},")
	      (point))))
    (browse-url (concat "x-bdsk://" (buffer-substring-no-properties s e)))))

;;; Finder に表示
(defun MyTool-show-in-finder ()
  (interactive)
  (process-kill-without-query
   (start-process-shell-command "Show in Finder" nil "open -R" (buffer-file-name))))

;;; Finder でフォルダを開く
;; ショートカット: ⌘-option-shift-F
;; (ショートカットは USキーボードで確認）
(define-key global-map [?\s-Ï] 'MyTool-open-folder-in-finder)
(defun MyTool-open-folder-in-finder ()
  "open -a Finder.app CURRENT-DIRECTORY"
  (interactive)
  (process-query-on-exit-flag 
   (start-process-shell-command "open folder in Finder" nil "open .")))

;;; Terminal/iTerm でフォルダを開く
;; ショートカット: ⌘-option-shift-T
;; (ショートカットは USキーボードで確認）
(define-key global-map [?\s-ˇ] 'MyTool-open-Terminal)
(defun MyTool-open-Terminal()
  "open -a Terminal.app CURRENT-DIRECTORY"
  (interactive)
  (let* (;;(cmd "open -a Terminal.app")
         (cmd "open -a iTerm.app"))
    (process-kill-without-query
     (start-process-shell-command 
      "Open directory" nil cmd default-directory))))

;;; BibDesk を開く
(defun MyTool-open-bibdesk ()
  (interactive)
  (process-kill-without-query
   (start-process-shell-command "Open BibDesk.app" nil "open -a BibDesk" )))

;;; \cite{foo} から foo.pdf を開く 
;;; citekey と PDFファイルの名前が同じで、PDFファイルが1つの場所に置かれている必要がある。
(defun MyTeX-open-article ()
  "Open PDF file by the cite key: \cite{foo} -> open foo.pdf"
  (interactive)
  (let* ((s (save-excursion
	      (skip-chars-backward "^{,")
	      (point)))
	 (e (save-excursion
	      (skip-chars-forward "^},")
	      (point)))
	 (pdfdir "~/Documents/Dropbox/Papers/pdf/")
	 (pdffile (concat pdfdir
			  (buffer-substring-no-properties s e) ".pdf"))
	 (process-kill-without-query
	  (start-process-shell-command "Open the article" nil "open" pdffile)))))

;;; テキスト（英語）の読み上げ(TeXに対応)
;;; Convert text to audible speech by /usr/bin/say (Emacs + OSX) 
(defun MyTeX-speech ()
  "Convert text to audible speech by /usr/bin/say of OSX."
  (interactive)
  (let (b e s
	  (block-sep-str "^\*\\|^\\ *%\\|^\\ *\n\\|\\\\item\\|\\\\begin\\|\\\\end"))
    (if (eq (process-status "speech") 'run)
	(delete-process "speech")
      (progn
	(if mark-active
	    (setq b (mark) e (point))
	  (setq b (save-excursion
		    (progn
		      (re-search-backward block-sep-str (point-min) 1)
		      (point)))
		e (save-excursion
		    (progn
		      (re-search-forward block-sep-str (point-max) 1)
		      (point)))))
	(setq s (buffer-substring-no-properties b e)
	      s (replace-regexp-in-string "\\\\%" "percent" s)
	      s (replace-regexp-in-string "$\\\\mu$m" "micrometer" s)
	      s (replace-regexp-in-string "\\\\item" " " s)
	      s (replace-regexp-in-string "\\\\begin" " " s)
	      s (replace-regexp-in-string "\\\\end" " " s)
	      s (replace-regexp-in-string "\\\\," " " s)
	      s (replace-regexp-in-string "%[^\n]*" "" s)
	      s (replace-regexp-in-string 
             "\\\\[a-zA-Z]+{\\|[$\\\\_^~{}`\"*\n]" " " s)
	      s (replace-regexp-in-string "\\ +" " " s))
	(message s)
	(process-kill-without-query
	 (start-process-shell-command "speech" nil
				      "/usr/bin/say" (concat "\"" s "\"" )))))))


;;; グローバルなキーバインドの設定
(define-key global-map (kbd "C-c k") 'MyTool-file-complete)
(define-key global-map (kbd "C-;") 'ispell-word)
(define-key global-map [?\s-O] 'MyTool-show-in-finder)
(define-key global-map [?\s-Ï] 'MyTool-open-folder-in-finder) 
(define-key global-map [?\M-\s-F] 'MyTool-open-folder-in-finder) ; option key = meta
(define-key global-map [?\s-˙] 'ns-do-hide-others)

(define-key global-map [?\s-r] 'MyTool-speech)
(define-key global-map [?\s-R] 'MyTool-speech-word)

(define-key global-map (kbd "C-c w") 'MyTool-lookup-dictionary-osx)
(define-key global-map (kbd "C-c g") 'MyTool-search-google)
(define-key global-map (kbd "C-c G") 'MyTool-search-googlescholar)




;;; YaTeX用キーバインドの設定
(add-hook 'yatex-mode-hook
          '(lambda ()
             (turn-off-auto-fill) ; 勝手に改行しない
	     (define-key YaTeX-mode-map [?\s-t] 
               (lambda 	() (interactive)
		 (require 'yatexprc)
		 (YaTeX-typeset-buffer)))
	     (define-key YaTeX-mode-map [?\s-b] 
               (lambda 	() (interactive)
		 (require 'yatexprc)
		 (YaTeX-typeset-buffer)))
             (define-key YaTeX-mode-map [?\s-P]
               (lambda 	() (interactive)
		 (require 'yatexprc)
		 (call-interactively 'YaTeX-preview)
		 ;;(YaTeX-preview "open -a Skim" YaTeX-parent-file)
		 ))
             (define-key YaTeX-mode-map [?\s-B] 
               (lambda 	() (interactive)
		 (require 'yatexprc)
                 (YaTeX-call-builtin-on-file "BIBTEX" bibtex-command)))
             (define-key YaTeX-mode-map [?\s-I]
               (lambda 	() (interactive)
		 (require 'yatexprc)
                 (YaTeX-call-builtin-on-file "MAKEINDEX" makeindex-command)))
	     (define-key YaTeX-mode-map [?\s-J] 'MyTeX-open-item-BibDesk)
             (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)
             (define-key YaTeX-mode-map "\t" 'latex-indent-command)
             (define-key YaTeX-mode-map (kbd "C-c TAB") 'latex-indent-region-command)
             (define-key YaTeX-mode-map [?\s-_] 'MyTeX-insert-subscript_rm)
             (define-key YaTeX-mode-map [?\C-\s-J] 'YaTeX-goto-corresponding-*)
             (define-key YaTeX-mode-map (kbd "C-c d") 'MyTeX-latexmk-cleanup)
             (define-key YaTeX-mode-map [?\s-H] 'YaTeX-display-hierarchy)
             (define-key YaTeX-mode-map [?\s-1] 'YaTeX-visit-main)
             (define-key YaTeX-mode-map [?\s-2] 'MyTeX-switch-to-previousbuffer)
             (define-key YaTeX-mode-map [?\M-\s-B] 'MyTool-open-bibdesk)
             (define-key YaTeX-mode-map [?\s-ı] 'MyTool-open-bibdesk)
             (define-key YaTeX-mode-map [?\M-\s-P] 'MyTeX-open-PreviewApp)
             (define-key YaTeX-mode-map [?\s-∏] 'MyTeX-open-PreviewApp)
             (define-key YaTeX-mode-map [?\s-\)] 'MyTeX-speech-region)
             (define-key YaTeX-mode-map [?\s-C] 'MyTeX-open-article)
             ))

