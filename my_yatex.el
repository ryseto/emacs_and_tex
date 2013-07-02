;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time-stamp: <2012-04-01 17:31:05 seto>
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
;;; option + command + H           : 実行されているその他すべてのアプリケーションのウインドウを隠す
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; テスト版の YaTeX を使っているのでそちらが優先してロードされるようにする。
;;; 現在 1.75.3 を使用中。
(setq load-path (cons (expand-file-name "~/.emacs.d/yatex") load-path))

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
(autoload 'latex-indent-command "~/.emacs.d/lisp/latex-indent"
  "Indent current line accroding to LaTeX block structure.")
(autoload 'latex-indent-region-command "~/.emacs.d/lisp/latex-indent"
  "Indent each line in the region according to LaTeX block structure.")

;;; シェルスクリプト SemiAutoTeX でタイプセット
;;; https://github.com/ryseto/emacs_and_tex/blob/master/semiautotex.sh
(setq tex-command "semiautotex.sh"
      bibtex-command "semiautotex.sh -b"
      makeindex-command "semiautotex.sh -i"
      YaTeX-typeset-auto-rerun nil ; rerun 機能を無効 (1.75.x 以降)
      dvi2-command "open -a Skim" ; PDF プレビュアとして Skim.app を使う
      )
;;; YaTeX カスタマイズ変数
(setq YaTeX-inhibit-prefix-letter t ; YaTeXキーバインドを C-c ? から C-c C-? に変更
      YaTeX-kanji-code nil ; 文字コードを変更しない
      YaTeX-use-AMS-LaTeX t ; amsmath を利用
      YaTeX-default-pop-window-height 7 ; タイプセットの時のウィンドウの高さ
      YaTeX-skip-default-reader  t ; 補完入力でミニバッファから入力しない
      YaTeX-latex-message-code 'utf-8
      YaTeX-template-file "~/Documents/Dropbox/template/template.tex" ; 新規作成時のテンプレート
      YaTeX::ref-labeling-section-level 3 ; ref 補完で subsection などを検索
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
(defun MyTeX-speech (b e)
  "Convert text to audible speech by /usr/bin/say of OSX."
  (interactive "r")
  (let (str)
    (if (eq mark-active 'nil)
	(progn (setq b (save-excursion (re-search-backward "^\\ *%")
				       (point)))
	       (setq e (save-excursion (re-search-forward "^\\ *%")
				       (point)))))
    (setq str (buffer-substring-no-properties b e))
    (setq str (replace-regexp-in-string "\\\\%" "percent" str))
    (setq str (replace-regexp-in-string "%[^\n]*" "" str))
    (setq str (replace-regexp-in-string "\\\\[a-zA-Z]+{" " " str))
    (setq str (replace-regexp-in-string "[$_^\n()~{}|;`]" " " str))
    (setq str (replace-regexp-in-string "\\ +" " " str))
    (message str)
    (if (eq (process-status "speech") 'run)
	(delete-process "speech"))
    (process-kill-without-query
     (start-process-shell-command "speech" nil 
				  "/usr/bin/say -r 250" (concat "\"" str "\"" )))))

;;; 少しだけスキップ
(defun MyTeX-jump-to-next ()
  (interactive)
  (cond
   ((= (following-char) ?$ )  (skip-chars-forward "$") )
   ((= (following-char) 40 ) (skip-chars-forward "(") )
   ((= (following-char) 41 ) (skip-chars-forward ")") )
   ((= (following-char) 91 ) (skip-chars-forward "["))
   ((= (following-char) 93 ) (skip-chars-forward "]"))
   (t
    (skip-chars-forward "^{}()[]\n\$")
    (skip-chars-forward "}")
    (skip-chars-forward "{")
    )))

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

(defun MyTool-search-google()
  "Search by google"
  (interactive)
  (let* ((str (string-word-or-region)))
    (browse-url
     (concat "http://google.com/search?q=\"" str "\""))))

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


;;; Finder に表示
(defun MyTool-show-in-finder ()
  (interactive)
  (process-kill-without-query
   (start-process-shell-command "Show in Finder" nil "open -R" (buffer-file-name))))

;;; Finder でフォルダを開く
(defun MyTool-open-folder-in-finder ()
  (interactive)
  (process-query-on-exit-flag 
   (start-process-shell-command "open folder in Finder" nil "open .")))

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

;;; クラッシュするキーバインドを無効にする。
(define-key global-map [?\s-p] nil) ; ns-print-buffer
(define-key global-map [?\s-S] nil) ; ns-write-file-using-panel
(define-key global-map [?\s-o] nil) ; ns-open-file-using-panel

;;; グローバルなキーバインドの設定
(define-key global-map (kbd "C-c k") 'MyTool-file-complete)
(define-key global-map (kbd "C-;") 'ispell-word)
(define-key global-map [?\s-O] 'MyTool-show-in-finder)
(define-key global-map [?\s-Ï] 'MyTool-open-folder-in-finder) 
(define-key global-map [?\M-\s-F] 'MyTool-open-folder-in-finder) ; option key = meta
(define-key global-map [?\s-˙] 'ns-do-hide-others)
(define-key global-map [?\s-r] 'MyTeX-speech)
(define-key global-map (kbd "C-c w") 'MyTool-lookup-dictionary-osx)
(define-key global-map (kbd "C-c g") 'MyTool-search-google)
(define-key global-map (kbd "C-c G") 'MyTool-search-googlescholar)

;;; YaTeX用キーバインドの設定
(add-hook 'yatex-mode-hook
          '(lambda ()
	     (require 'yatexprc)
	     (turn-off-auto-fill) ; 勝手に改行しない
	     (define-key YaTeX-mode-map [?\s-t] 'YaTeX-typeset-buffer)
	     (define-key YaTeX-mode-map [?\s-b] 'YaTeX-typeset-buffer)
	     (define-key YaTeX-mode-map [?\s-P] 'YaTeX-preview)
	     (define-key YaTeX-mode-map [?\s-R] 'skim-forward-search)
	     (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)
	     (define-key YaTeX-mode-map [?\s-B] 
	       (lambda 	() (interactive)
		 (YaTeX-call-builtin-on-file "BIBTEX" bibtex-command)))
	     (define-key YaTeX-mode-map [?\s-I]
	       (lambda 	() (interactive)
		 (YaTeX-call-builtin-on-file "MAKEINDEX" makeindex-command)))
	     (define-key YaTeX-mode-map "\t" 'latex-indent-command)
	     (define-key YaTeX-mode-map (kbd "C-c TAB") 'latex-indent-region-command)
	     (define-key YaTeX-mode-map [?\s-_] 'MyTeX-insert-subscript_rm)
	     (define-key YaTeX-mode-map [?\C-\s-J] 'YaTeX-goto-corresponding-*)
	     (define-key YaTeX-mode-map (kbd "C-c d") 'MyTeX-latexmk-cleanup)
	     (define-key YaTeX-mode-map (kbd "C-c j") 'MyTeX-jump-to-next)
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

