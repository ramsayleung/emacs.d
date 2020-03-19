					; package --- Summary  -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:
;;;----------------;;;
;;; User Interface ;;;

;;; Ignore case when searching
(setq case-fold-search t)


;;;------------------;;;
;;; Windows & Frames ;;;
;;;------------------;;;

;; language
;;; always split windows horizontally rather than vertically
;; (setq split-height-threshold nil)
(setq current-language-environment "English")

;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
(use-package popwin
  :ensure t
  :config (progn
	    (run-with-idle-timer ramsay-idle-time nil 'popwin-mode)
	    ;; (popwin-mode t)
	    (push '(compilation-mode :noselect t) popwin:special-display-config)
	    ;; (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
	    (push "*slime-apropos*" popwin:special-display-config)
	    (push "*slime-macroexpansion*" popwin:special-display-config)
	    (push "*slime-description*" popwin:special-display-config)
	    (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
	    (push "*slime-xref*" popwin:special-display-config)
	    (push '(sldb-mode :stick t) popwin:special-display-config)
	    (push 'slime-repl-mode popwin:special-display-config)
	    (push 'slime-connection-list-mode popwin:special-display-config)
	    (push "*vc-diff*" popwin:special-display-config)
	    (push "*vc-change-log*" popwin:special-display-config)
	    (push '("*Youdao Dictionary*" :noselect t :width 0.2 :position bottom) popwin:special-display-config)
	    (push '("*Help*" :position bottom :stick t :height 0.5) popwin:special-display-config)
	    ))


;;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun ramsay/toggle-window-split ()
  "Vertical split show more of each line, horizontal split show more lines.
This code toggles between them."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; Specify the fringe width for windows -- this sets the left to 10 and
;; right fringes to 0
(fringe-mode '(0 . 0))
;; (when window-system
;; Turn off tool bar
(tool-bar-mode -1)
;; Turn off file scroll bar
(scroll-bar-mode -1)
;; Turn off title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; Assuming you are using a dark theme
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
;;; Disable mouse scrolling
(mouse-wheel-mode -1)
;; )

;;; 设置中英文等高字体设置. 等高与等宽, 两者只能其一. 如果想设置等宽, 将
;;; WenQuanYi 的 size 设置为 16.5
(defun ramsay/set-font ()
  "Set font."
  (interactive)
  (message "Update font configuration.")
  (if window-system
      (progn
	(set-face-attribute 'mode-line nil :font "Fantasque Sans Mono-16:weight=medium:slant=italic")
	(set-face-attribute
	 'default nil
	 :font (font-spec :name "-PfEd-Fantasque Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
			  :weight 'normal
			  :slant 'italic
			  :size 16.0))
	(let ((chinese-font (if (ramsay/mac-os-p)
				"-WQYF-Microsoft YaHei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"
			      "-WQYF-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")))
	  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	    (set-fontset-font
	     (frame-parameter nil 'font)
	     charset
	     (font-spec :name chinese-font
			:weight 'normal
			:slant 'normal
			:size 15.2))))
	)
    (add-to-list 'default-frame-alist
		 '(font . "Fantasque Sans Mono-16:weight=medium:slant=italic"))))

(defun ramsay/set-font-at-time ()
  "Set font with `run-at-time`."
  (run-at-time "5 sec" nil 'ramsay/set-font))

(ramsay/set-font)

;;; Change vertical-border for terminal Emacs.
;;; Vertical-border in terminal is ugly, fix it.
(if (not (eq window-system 'nil))
    (progn
;;; When `scroll-bar-mode` is enabled, vertical-border is controlled implictly
;;; by `scroll-bar-mode`.
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Scroll-Bars.html#Scroll-Bars
;;; Change vertical-border for GUI Emacs.
      (setq window-divider-default-right-width 2)
      (window-divider-mode)
      )
  (progn
    (set-face-inverse-video 'vertical-border nil)
    (set-face-background 'vertical-border (face-background 'vertical-border))
    ;; ;; Set symbol for the border
    (set-display-table-slot standard-display-table
                            'vertical-border
                            (make-glyph-code ?┃))
    )
  )

;; no menubar
(menu-bar-mode -1)
;; turn off startup help menu
(setq inhibit-splash-screen t)

;;; Use default line-number-mode instead of nlinum or linum (require Emacs >= 26).
;; (setq-default display-line-numbers-width 1)
(setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode t)

;; number of characters until the fill column
(setq fill-column 120)
;; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)

;; Disable line wrap '\'
(global-visual-line-mode)
(setq line-move-visual t)
(set-display-table-slot standard-display-table 'wrap ?\ )
;;; make sure my code stays within 100 characters always
;;; and prefer the soft line wrap while writing prose
;;; https://www.emacswiki.org/emacs/TruncateLines
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)
;; make emacs full-screen at startup

(toggle-frame-maximized)
;;; show customized phrase in scratch
;;; From Purcell https://github.com/purcell/emacs.d
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;;----------;;
;;  Cursor  ;;
;;----------;;
;; highlight current line
(global-hl-line-mode 1)
;; don't blink the cursor
(blink-cursor-mode -1)

;; Only use `bar' type of cursor shape
(setq cursor-type 'box)
(add-hook 'minibuffer-setup-hook '(lambda () (setq cursor-type 'box)))
(add-hook 'eshell-mode-hook '(lambda () (setq cursor-type 'box)))

;; make sure transient mark mode is enabled (it should be by default,
;; but just in case)
(transient-mark-mode t)
;; turn on mouse wheel support for scrolling
;; (mouse-wheel-mode t)

;;; Turn-off Alarm Bell
(setq ring-bell-function #'ignore)
;;; Use visible bell instead of buzzer
(setq visible-bell t)

;;  Color Theme  ;;
;;---------------;;

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package spacemacs-theme
  :ensure t
  :defer t)

;;; Disable theme before load a new theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  "Disable theme before load theme."
  (mapc #'disable-theme custom-enabled-themes))
(load-theme 'zenburn t)

;;----------------;;
;;Major/Minor Mode;;
;;----------------;;

;;; customize default mode line
;;; disable status of "read only" or "wriable"
(setq-default mode-line-modified nil)
(setq-default mode-line-frame-identification nil)
(setq-default mode-line-remote nil)

;;;Move evil tag to beginning of mode line
(setq evil-mode-line-format '(before . mode-line-front-space))

(use-package diminish
  :ensure t
  :demand t
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish visual-line-mode
  :diminish auto-revert-mode
  :diminish auto-fill-function
  :diminish mail-abbrevs-mode
  :diminish highlight-indentation-mode
  :diminish visual-line-mode
  :diminish subword-mode)

;;; Stolen From https://github.com/hrs/dotfiles/blob/master/emacs.d/configuration.org
(defmacro diminish-minor-mode (filename mode &optional abbrev)
  "Macro for diminish minor mode with FILENAME MODE and ABBREV."
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  "Macro for diminish major mode with MODE-HOOK and ABBREV."
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))
(diminish-minor-mode 'highlight-indentation 'highlight-indentation-mode )
(diminish-minor-mode 'mail-abbrevs 'mail-abbrevs-mode )
(diminish-minor-mode 'auto-revert 'auto-revert-mode)
(diminish-minor-mode 'simple 'auto-fill-function )
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-major-mode 'emacs-lisp-mode-hook "Elisp")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")

(message "loading init-ui")
(provide 'init-ui)

;;; init-ui.el ends here
