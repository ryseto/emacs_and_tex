;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time-stamp: <2013-07-03 11:09:59 seto>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; fundamental configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; path setting
(setq exec-path
      (append
       (list "~/Dropbox/bin" "/usr/local/bin" "/usr/osxws/bin/" ) exec-path))

(setenv "PATH" 
        (concat '"~/Dropbox/bin:/usr/local/bin:/usr/osxws/bin" (getenv "PATH")))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;;; save the position before you editing.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/Library/Application Support/emacs-places.txt")

;;; copy foo to foo~ as a backup file
(setq backup-by-copying t)

;;; deleting files goes to OS's trash folder
;;(setq delete-by-moving-to-trash t)
;;(setq trash-directory "~/.Trash")

;;; start emacsclient server
(require 'server)
(unless (server-running-p) (server-start))

(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Section 1 language configurations (for Japanese)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; japanese settings for Cocoa Emacs
(set-language-environment 'Japanese)
(prefer-coding-system  'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; appearance setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; hide tool-bar and menu-bar
(if window-system
    (tool-bar-mode 0)
  (menu-bar-mode 0))

;;; show the corresponding paren 
(show-paren-mode)
  
;;; do not font scaling
(setq scalable-fonts-allowed nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Section 3 keyboard/keybinding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; input special and control characters by "Option"
(setq ns-option-modifier 'none)

;;; emulation of the standard CUA key bindings (Mac GUI)
(cua-selection-mode t)
  
;;; behavior of "Command + Cursor" to the default of MacOS X
;;; default : ns-next-frame in ns-win.el
(define-key global-map [s-left] 'move-beginning-of-line)
;;; default : ns-prev-frame in ns-win.el
(define-key global-map [s-right] 'move-end-of-line)
(define-key global-map [s-up] 'backward-page)
(define-key global-map [s-down] 'forward-page)
  
;;; font resize short cut (Command +/-/0)
(global-set-key [(s ?+)] (lambda () (interactive) (text-scale-increase 1)))
(global-set-key [(s ?-)] (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key [(s ?0)] (lambda () (interactive) (text-scale-increase 0)))
  
;;; revert [Home] Key and [End] Key
(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)

;;; Delete the following character by fn + delete 
(define-key global-map [kp-delete] 'delete-char)

;;; fix yen key problem on JIS keyboard
;;; Ando-san's code (see [Macemacsjp-users 1126])
(define-key global-map [2213] nil)
(define-key global-map [67111077] nil)
(define-key function-key-map [2213] [?\\])
(define-key function-key-map [67111077] [?\C-\\]) 

(define-key global-map [3420] nil)
(define-key global-map [67112284] nil)
(define-key function-key-map [3420] [?\\])
(define-key function-key-map [67112284] [?\C-\\])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; shell-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide password 
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;;; escape sequence
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Cocoa Emacs window mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
    ;;;; Font setting
    ;;; if display-height is less than 900, set font size 12pt.
  (let* ((size (if (< (display-pixel-height) 900) 12 14))
         (asciifont "Menlo")
         (jpfont "Hiragino Maru Gothic ProN")
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; half-width KaTaKaNa
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; Accented Latin
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; Greek
    )
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3))))

(when window-system
  ;;(set-frame-font "Inconsolata 16")
  ;;  (set-frame-font "Menlo 14")
  (set-frame-font "Courier New 14")
  (set-fontset-font (frame-parameter nil 'font)
                    'unicode
                    '("ヒラギノ角ゴ ProN" . "unicode-bmp") nil 'append)
  (add-to-list 'default-frame-alist '(font . "Courier New 14"))
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3)))
)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; anything else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The number of lines to scroll a window by when point moves out.
(setq scroll-step 1)

;;;; Time Stamp
;;;   If you put 'Time-stamp: <>' or 'Time-stamp: ""' on
;;;   top 8 lines of the file, the '<>' or '""' are filled with the date
;;;   at saving the file.
(require 'time-stamp)
(if (not (memq 'time-stamp write-file-functions))
    (setq write-file-functions
          (cons 'time-stamp write-file-functions)))

;;; ispell のユーザー辞書を Dropbox 内に。
(setq ispell-personal-dictionary "~/Dropbox/emacs/aspell.en.pws")
(setq ispell-dictionary "en_US")

;;--------------------------------------------------------------------
;; Lines enabling gnuplot-mode
;; move the files gnuplot.el to someplace in your lisp load-path or
;; use a line like

;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode 
(global-set-key [(f9)] 'gnuplot-make-buffer)

(setq gnuplot-program "/usr/osxws/bin/gnuplot")

(add-hook 'gnuplot-mode-hook '(lambda () 
     (define-key gnuplot-mode-map [?\s-b] 'gnuplot-send-region-to-gnuplot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; buffer-file-coding-system: utf-8-unix
;; End: