;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time-stamp: <2012-02-28 14:41:26 seto>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド
;;; === YaTeX ===
;;; Cmd-T and Cmd-B         : タイプセット
;;; Cmd-P and Shift-Cmd-P   : プレビュー
;;; Shift-Cmd-B             : bibtex
;;; Shift-Cmd-I             : makeindex
;;; Tab                     : インデント (latex-indent)
;;; C-c Tab                 : 領域をインデント (latex-indent)
;;; C-c s                   : Skim PDF カーソル位置表示
;;; C-c d                   : latexmk -c を実行
;;; Cmd-_                   : "_{\mathrm{}}" を挿入
;;; === グローバル ===
;;; C-c w                   : OSX の辞書で調べる
;;; C-c k                   : ファイル名の補完
;;; C-;                     : スペルチェック
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; テスト版の YaTeX を使う。
(setq load-path (cons (expand-file-name "~/.emacs.d/yatex") load-path))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))

;;; インデント
;;; (YaTeXのインデントを使わない）
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
      dvi2-command "open -a Skim")

(setq YaTeX-inhibit-prefix-letter t ; C-c C- .... 
      YaTeX-kanji-code nil
      YaTeX-use-AMS-LaTeX t ; amsmath を利用
      YaTeX-default-pop-window-height 7 ; タイプセットの時のウィンドウの高さ
      YaTeX-skip-default-reader  t ; 補完入力でミニバッファから入力しない。
      YaTeX-latex-message-code 'utf-8
      YaTeX::ref-labeling-section-level 3 ; ref 補完で subsection などを検索
      )

;;; Skim PDF カーソル位置表示
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
(defun my-file-complete ()
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
(defun my-osx-dictionary ()
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

(add-hook 'yatex-mode-hook
          '(lambda ()
	     (require 'yatexprc)
	     (turn-off-auto-fill) ; 勝手に改行しない
	     (define-key YaTeX-mode-map [(s t)] 'YaTeX-typeset-buffer)
	     (define-key YaTeX-mode-map [(s b)] 'YaTeX-typeset-buffer)
	     (define-key YaTeX-mode-map [(s p)] 'YaTeX-preview)
	     (define-key YaTeX-mode-map [(s shift p)] 'YaTeX-preview)
	     (define-key YaTeX-mode-map [(s shift b)]
	       (lambda 	() (interactive)
		 (YaTeX-call-builtin-on-file "BIBTEX" bibtex-command)))
	     (define-key YaTeX-mode-map [(s shift i)] 
	       (lambda 	() (interactive)
		 (YaTeX-call-builtin-on-file "MAKEINDEX" makeindex-command)))
	     (define-key YaTeX-mode-map "\t" 'latex-indent-command)
	     (define-key YaTeX-mode-map (kbd "C-c TAB") 'latex-indent-region-command)
	     (define-key YaTeX-mode-map [(s _)] 'MyTeX-insert-subscript_rm)
	     (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)
	     (define-key YaTeX-mode-map (kbd "C-c d") 'MyTeX-latexmk-cleanup)
	     (define-key YaTeX-mode-map (kbd "C-c j") 'MyTeX-jump-to-next)
	     ))

(define-key global-map (kbd "C-c w") 'my-osx-dictionary)
(define-key global-map (kbd "C-c k") 'my-file-complete)
(define-key global-map (kbd "C-;") 'ispell-word)
