;;; package --- Summary
;;; Code:
;;; Commentary:
;;;----------------;;;
;;; User Interface ;;;
;;;----------------;;;

;;;ignore case when searching
(setq case-fold-search t)

;;;------------------;;;
;;; Windows & Frames ;;;
;;;------------------;;;
;; language
(setq current-language-environment "English")

;;; automatic long line wrapping
(setq-default auto-fill-function 'do-auto-fill)

;;turn off tool bar
(tool-bar-mode -1)
;; turn off file scroll bar
(scroll-bar-mode -1)
;; no menubar
(menu-bar-mode -1)
;; turn off startup help menu
(setq inhibit-splash-screen t)
;; show line number
(global-linum-mode t)
;; number of characters until the fill column
(setq fill-column 80)
;; specify the fringe width for windows -- this sets both the left and
;; right fringes to 10
(fringe-mode 10)
;; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)
;;; make sure my code stays within 100 characters always and prefer the soft line wrap while writing prose
;;; https://www.emacswiki.org/emacs/TruncateLines
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)
;; make emacs full-screen at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;----------;;
;;  Cursor  ;;
;;----------;;
;; highlight current line
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
(setq hl-line-face (quote highlight))

;; don't blink the cursor
(blink-cursor-mode nil)

;;set the cursor type to bar
(setq-default cursor-type 'bar)
;; make sure transient mark mode is enabled (it should be by default,
;; but just in case)
(transient-mark-mode t)
;; turn on mouse wheel support for scrolling
(mouse-wheel-mode t)

;;----------------------;;
;; Syntax Highlighting  ;;
;;----------------------;;
;; text decoration
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-verbose t)
;; if there is size information associated with text, change the text
;; size to reflect it
(size-indication-mode t)

;; highlight parentheses when the cursor is next to them
(show-paren-mode t)
;;---------------;;
;;  Color Theme  ;;
;;---------------;;
;; (use-package dracula-theme
;;   :ensure t
;;   )
;; (use-package monokai-theme
;;   :ensure t
;;   )
(use-package zenburn-theme
  :ensure t
  :config(load-theme 'zenburn t))
(use-package powerline
  :ensure t
  :config(progn
	   (powerline-default-theme)
	   (setq powerline-default-separator 'nil))
  )

;;---------------;;
;;      Font     ;;
;;---------------;;

;; customize font
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-12"))


;;----------------;;
;;Major/Minor Mode;;
;;----------------;;
;;; Use Miminish minor modes to change the mode line
;;; The mode line map:
;;; paredit-mode -> "(e)"
;;; wakatime-mode -> "ω"
;;; smarthparence-mode -> "(s)"
;;; hungrydelete-mode -> "hd"
;;; abbrev-mode -> ""  "" means hide this minor mode from mode line
;;; undo-tree-mode -> ""
;;; whichkey-mode -> ""
(use-package diminish
  :ensure t
  :demand t
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish mail-abbrevs-mode
  :diminish subword-mode)

(provide 'init-ui)
;;; init-ui.el ends here
