;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ~/.yatex.el for MacOS X WorkShop
;;            KOBAYASHI Taizo <xxxxxxxxx@xxxxxx.com>
;; Time-stamp: <2012-02-27 18:02:50 seto>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yatex $B$N@_Dj(B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; $B%F%9%HHG$N(B YaTeX $B$r;H$&!#(B
(setq load-path (cons (expand-file-name "~/.emacs.d/yatex") load-path))

(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)


;; $B%9%/%j%W%H(B SemiAutoTeX $B$G%?%$%W%;%C%H(B
(setq tex-command "semiautotex.sh"
      bibtex-command "semiautotex.sh -b"
      makeindex-command "semiautotex.sh -i"
      YaTeX-typeset-auto-rerun nil ;; for 1.75.xx 
      dvi2-command "open -a Skim")


(setq YaTeX-inhibit-prefix-letter t ;; C-c C-.... 
      YaTeX-kanji-code nil
      YaTeX-use-AMS-LaTeX t
      YaTeX-default-pop-window-height 7
      YaTeX-skip-default-reader  t ;; $BJd40F~NO$G%_%K%P%C%U%!$+$iF~NO$7$J$$!#(B
      YaTeX-latex-message-code 'utf-8
      YaTeX::ref-labeling-section-level 3 ;; ref $BJd40$G(B subsection $B$J$I$r8!:w(B
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



(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)))

(defun MyTeX-latexmk-cleanup ()
  (interactive)
  (message "cleaning up all nonessential files...")
  (process-query-on-exit-flag 
   (start-process-shell-command "latexmk clean-up" nil "latexmk -c")))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c d") 'MyTeX-latexmk-cleanup)))

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

(autoload 'latex-indent-command "~/.emacs.d/lisp/latex-indent"
  "Indent current line accroding to LaTeX block structure.")

(autoload 'latex-indent-region-command "~/.emacs.d/lisp/latex-indent"
  "Indent each line in the region according to LaTeX block structure.")

(add-hook 'yatex-mode-hook
          '(lambda ()
	     (auto-fill-mode 0)
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
	     (define-key YaTeX-mode-map [(kbd "C-c l")]
					 'texlabel-auto-labeling-default)

	     (local-set-key "\t" 'latex-indent-command)
	     (local-set-key [(s _)] 'MyTeX-insert-subscript_rm)
	     (local-set-key [(control \])] 'MyTeX-jump-to-next)
	     (kill-local-variable 'mac-ignore-shortcut)
	     )
)

