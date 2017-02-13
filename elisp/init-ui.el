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
;;; show customized phrase in scratch
;;; From Purcell https://github.com/purcell/emacs.d
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

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
(blink-cursor-mode -1)

;;set the cursor shape to bar
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


;;---------------;;
;;      Font     ;;
;;---------------;;

;; customize font
(cond ((eq system-type 'gnu/linux)
       (set-frame-font "Source Code Pro-12"))
      ((eq system-type 'darwin)
       (set-frame-font "Monaco"))
      ((eq system-type 'windows-nt)
       (set-frame-font "Consolas")))


;;----------------;;
;;Major/Minor Mode;;
;;----------------;;

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'nil)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )
;;; Use Miminish minor modes to change the mode line
;;; The mode line map:
;;; paredit-mode -> "π"
;;; wakatime-mode -> "ω"
;;; flycheck-mode -> "φ"
;;; smarthparence-mode -> "(s)"
;;; hungrydelete-mode -> ""
;;; lisp-interaction-mode -> "λ"
;;; abbrev-mode -> ""  "" means hide this minor mode from mode line
;;; undo-tree-mode -> ""
;;; whichkey-mode -> ""
(use-package diminish
  :ensure t
  :demand t
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-revert-mode
  :diminish auto-fill-function
  :diminish mail-abbrevs-mode
  :diminish subword-mode)
;;; Stolen From https://github.com/hrs/dotfiles/blob/master/emacs.d/configuration.org
(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))
(diminish-minor-mode 'mail-abbrevs 'mail-abbrevs-mode )
(diminish-minor-mode 'simple 'auto-fill-function )
(diminish-minor-mode 'eldoc 'eldoc-mode)
;; (diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
;; (diminish-minor-mode 'subword 'subword-mode)
(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")
(provide 'init-ui)
;;; init-ui.el ends here
