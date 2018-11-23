(setq TeX-PDF-mode t)



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

(defun string-word-or-region ()
  "If region is selected, this returns the string of the region. If not, this returns the string of the word on which the cursor is."
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
(define-key global-map [?\s-r] 'MyTeX-speech)
(define-key global-map (kbd "C-c w") 'MyTool-lookup-dictionary-osx)
(define-key global-map (kbd "C-c g") 'MyTool-search-google)
(define-key global-map (kbd "C-c G") 'MyTool-search-googlescholar)
(define-key global-map [?\s-r] 'MyTeX-speech)

;;; YaTeX用キーバインドの設定
(add-hook 'yatex-mode-hook
          '(lambda ()
             (require 'yatexprc)
             (turn-off-auto-fill) ; 勝手に改行しない
             (define-key YaTeX-mode-map [?\s-t] 'YaTeX-typeset-buffer)
             (define-key YaTeX-mode-map [?\s-b] 'YaTeX-typeset-buffer)
             (define-key YaTeX-mode-map [?\s-P] 'YaTeX-preview)
             (define-key YaTeX-mode-map [?\s-R] 'skim-forward-search)
	     (define-key YaTeX-mode-map [?\s-J] 'MyTeX-open-item-BibDesk)
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

