;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time-stamp: <2012-02-27 18:50:48 seto>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; テスト版の YaTeX を使う。
(setq load-path (cons (expand-file-name "~/.emacs.d/yatex") load-path))

(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

(autoload 'latex-indent-command "~/.emacs.d/lisp/latex-indent"
  "Indent current line accroding to LaTeX block structure.")

(autoload 'latex-indent-region-command "~/.emacs.d/lisp/latex-indent"
  "Indent each line in the region according to LaTeX block structure.")

;; スクリプト SemiAutoTeX でタイプセット
(setq tex-command "semiautotex.sh"
      bibtex-command "semiautotex.sh -b"
      makeindex-command "semiautotex.sh -i"
      YaTeX-typeset-auto-rerun nil ;; for 1.75.xx rerun 機能を無効
      dvi2-command "open -a Skim")

(setq YaTeX-inhibit-prefix-letter t ;; C-c C- .... 
      YaTeX-kanji-code nil
      YaTeX-use-AMS-LaTeX t
      YaTeX-default-pop-window-height 7 ;; タイプセットの時のウィンドウの高さ
      YaTeX-skip-default-reader  t ;; 補完入力でミニバッファから入力しない。
      YaTeX-latex-message-code 'utf-8
      YaTeX::ref-labeling-section-level 3 ;; ref 補完で subsection などを検索
      )

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

(defun MyTeX-latexmk-cleanup ()
  (interactive)
  (let* ((x (read-string "cleaning up all nonessential files... (y/n):")))
    (if (string= x "y")
	(process-query-on-exit-flag 
	 (start-process-shell-command "latexmk clean-up" nil "latexmk -c")))))

(defun MyTeX-insert-subscript_rm ()
  (interactive)
  (insert "_{\\mathrm{}}")
  (backward-char 2))

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

(add-hook 'yatex-mode-hook
          '(lambda ()
	     (require 'yatexprc)
	     (turn-off-auto-fill)
	     (define-key YaTeX-mode-map [(s t)] 'YaTeX-typeset-buffer)
	     (define-key YaTeX-mode-map [(s b)] 'YaTeX-typeset-buffer)
	     (define-key YaTeX-mode-map [(s p)] 'YaTeX-preview)
	     (define-key YaTeX-mode-map [(s shift p)] 'YaTeX-preview)
	     (define-key YaTeX-mode-map [(s shift b)]
	       (lambda 	() (interactive)
		 (YaTeX-call-builtin-on-file
		  "BIBTEX" bibtex-command)))
	     (define-key YaTeX-mode-map [(s shift i)] 
	       (lambda 	() (interactive)
		 (YaTeX-call-builtin-on-file
		  "MAKEINDEX" makeindex-command)))
	     (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)
	     (define-key YaTeX-mode-map "\t" 'latex-indent-command)
	     (define-key YaTeX-mode-map [(s _)] 'MyTeX-insert-subscript_rm)
	     (define-key YaTeX-mode-map (kbd "C-c j") 'MyTeX-jump-to-next)
	     (define-key YaTeX-mode-map (kbd "C-c d") 'MyTeX-latexmk-cleanup)
	     ))
