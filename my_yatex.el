;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time-stamp: <2012-03-11 18:50:47 seto>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド
;;; === YaTeX ===
;;; command + T / command + B      : タイプセット
;;; shift + command + P            : プレビュー
;;; shift + command R / C-c s      : Skim PDF カーソル位置表示
;;; shift + command + B            : bibtex
;;; shift + command + I            : makeindex
;;; command + 1                    : メインファイルのバッファを開く
;;; command + 2                    : 1つ前のバッファを開く
;;; Tab                            : インデント (latex-indent)
;;; C-c + Tab                      : 領域をインデント (latex-indent)
;;; C-c + d                        : latexmk -c を実行
;;; command + "_"                  : "_{\mathrm{}}" を挿入
;;; === グローバル ===
;;; C-c w                          : OSX の辞書で調べる
;;; C-c k                          : ファイル名の補完
;;; C-;                            : スペルチェック
;;; shift + command + O            : Finder に表示
;;; option + shift + command + F   : Finder でフォルダを開く
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq YaTeX-inhibit-prefix-letter t ; C-c C- ....
      YaTeX-kanji-code nil ; 文字コードを変更しない
      YaTeX-use-AMS-LaTeX t ; amsmath を利用
      YaTeX-default-pop-window-height 7 ; タイプセットの時のウィンドウの高さ
      YaTeX-skip-default-reader  t ; 補完入力でミニバッファから入力しない
      YaTeX-latex-message-code 'utf-8
      YaTeX-template-file "~/Documents/Dropbox/template/template.tex" ; 新規作成時のテンプレート
      YaTeX::ref-labeling-section-level 3 ; ref 補完で subsection などを検索
      )

;;; begin型、C-b 1文字のカスタマイズ
(setq yatex-mode-load-hook
      '(lambda() 
	 (YaTeX-define-begend-key "be" "equation")
	 (YaTeX-define-begend-key "bE" "enumerate")
	 (YaTeX-define-begend-key "ba" "align")
	 (YaTeX-define-begend-key "bg" "gather")
	 (YaTeX-define-begend-key "bf" "figure")
	 ))

;;;; Skim PDF カーソル位置表示
;;; pdflatex/platex -synctex=1 
;;; Emacs から Skim へ C-c 
(defun skim-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "/Applications/Skim.app/Contents/SharedSupport/displayline")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat ln " " pf " " ctf))
    (message args)
    (process-kill-without-query
     (start-process-shell-command "displayline" nil cmd args))))

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

;;; Romanな下付き添え字
(defun MyTeX-insert-subscript_rm ()
  (interactive)
  (insert "_{\\mathrm{}}")
  (backward-char 2))

;;; 1つ前の buffer に切り替える
(defun MyTeX-switch-to-previousbuffer ()
  (interactive)
  (switch-to-buffer nil))

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

;;; OSX の辞書で調べる(Sakito さん)
;;; http://sakito.jp/mac/dictionary.html
(defun MyTool-osx-dictionary ()
  "dictionary.app"
  (interactive)
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
        (setq beg (progn (backward-word) (point)))
        ))
    (browse-url
     (concat "dict:///"
             (url-hexify-string (buffer-substring-no-properties beg end))))))

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

;;; クラッシュするキーバインドを無効にする。
(define-key global-map [?\s-p] nil) ; ns-print-buffer
(define-key global-map [?\s-S] nil) ; ns-write-file-using-panel
(define-key global-map [?\s-o] nil) ; ns-open-file-using-panel

;;; グローバルなキーバインドの設定
(define-key global-map (kbd "C-c w") 'MyTool-osx-dictionary)
(define-key global-map (kbd "C-c k") 'MyTool-file-complete)
(define-key global-map (kbd "C-;") 'ispell-word)
(define-key global-map [?\s-O] 'MyTool-show-in-finder)
(define-key global-map [?\M-\s-F] 'MyTool-open-folder-in-finder)

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

	     (define-key YaTeX-mode-map (kbd "C-c d") 'MyTeX-latexmk-cleanup)
	     (define-key YaTeX-mode-map (kbd "C-c j") 'MyTeX-jump-to-next)
	     (define-key YaTeX-mode-map [?\s-1] 'YaTeX-visit-main)
	     (define-key YaTeX-mode-map [?\s-2] 'MyTeX-switch-to-previousbuffer)
	     ))


;;;; TeX ファイルを一括で開き YaTeX-parent-file を設定する
;;; TeX ファイルのディレクトリ内に mytexconfig というファイルを用意し、
;;; MyTeX-file-list にファイル名を連ねる。
;;;  (setq MyTeX-file-list
;;;      '("foo_main.tex" "foo_included_1.tex" "foo_included_2.tex" ))
;;; TeX ファイルを開く時にこのリストの先頭にあるファイルを YaTeX-parent-file として
;;; 他の全てのファイルも同時にバッファに開く。
;;; (YaTeX 標準の機能でも、C-c C-d に割り当てられている
;;; YaTeX-display-hierarchy を実行すると input/include されているファイルの
;;; 一覧が見れると同時にそれらを一括で開くことができる。)

(defun MyTeX-initial-setup ()
  (interactive)
  (if (file-exists-p "mytexconfig")
      (progn
	(load-file "mytexconfig")
	(dolist (file MyTeX-file-list)
	  (find-file-noselect file))
	(setq YaTeX-parent-file (car MyTeX-file-list))
	(message "MyTeX: initial setup done.")
	)))

(add-hook 'yatex-mode-hook
      '(lambda()
	 (MyTeX-initial-setup)))
