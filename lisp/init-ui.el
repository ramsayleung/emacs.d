;;; package --- Summary  -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:
;;;----------------;;;
;;; User Interface ;;;

;;; Ignore case when searching
(setq case-fold-search t)


;;;------------------;;;
;;; Windows & Frames ;;;
;;;------------------;;;

;;; language
;;; always split windows horizontally rather than vertically
;; (setq split-height-threshold nil)
(setq current-language-environment "English")

;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
(use-package popwin
  :ensure t
  :config (progn
	    (popwin-mode t)
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
	    (push '("*Help*" :position bottom :stick t :height 0.5) popwin:special-display-config)
	    ))

(use-package writeroom-mode
  :hook (emacs-startup . global-writeroom-mode)
  :ensure t
  :config
  (setq writeroom-width 128
	writeroom-maximize-window nil
	writeroom-fullscreen-effect 'maximized
	writeroom-major-modes '(org-mode markdown-mode))
  (add-hook 'writeroom-mode-hook (lambda () (display-line-numbers-mode -1)))
  )

(defun ramsay/hide-bar ()
  "Hide bar."
  (when window-system
    ;; Specify the fringe width for windows -- this sets the left to 10 and
    ;; right fringes to 0
    (fringe-mode '(0 . 0))
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
    ))
(ramsay/hide-bar)

;;; 设置中英文等高字体设置. 等高与等宽, 两者只能其一. 如果想设置等宽, 将
;;; WenQuanYi 的 size 设置为 16.5
(defun ramsay/set-chinese-font (&optional arg)
  "Set font."
  (interactive)
  (when window-system
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :name "-WQYF-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"
		  :weight 'normal
		  :slant 'normal
		  :height arg)))))

(add-to-list 'default-frame-alist
	     '(font . "-PfEd-Fantasque Sans Mono-regular-normal-normal-*-19-*-*-*-m-0-iso10646-1"))

;;; Apply text-scale-adjust for all buffer
(defadvice text-scale-increase (around all-buffers (arg) activate)
  "Text scale adjust for all buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

(defun ramsay/set-font-at-time ()
  "Set font with `run-at-time`."
  (run-with-timer 2 nil 'ramsay/set-chinese-font)
  (run-with-timer 2 nil 'ramsay/hide-bar)
  )
(ramsay/set-chinese-font 16)

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

;;; Turn-off Alarm Bell
(setq ring-bell-function #'ignore)
;;; Use visible bell instead of buzzer
(setq visible-bell t)

;;  Color Theme  ;;
;;---------------;;

(use-package acme-theme
  :ensure t
  :init
  :defer t)

;;; Disable theme before load a new theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  "Disable theme before load theme."
  (mapc #'disable-theme custom-enabled-themes))

;; (load-theme 'modus-operandi t)
(load-theme 'acme t)

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
  :diminish terminal-focus-reporting-mode
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
(diminish-major-mode 'lisp-interaction-mode-hook "Lisp")
(diminish-major-mode 'python-mode-hook "Py")

(message "loading init-ui")
(provide 'init-ui)

;;; init-ui.el ends here
