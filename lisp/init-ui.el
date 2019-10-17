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
	    (run-with-idle-timer samray-idle-time nil 'popwin-mode)
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

(defun samray/toggle-golden-ratio()
  "Check whether turn on golden ratio."
  (or (< (display-pixel-height)800)
      (< (display-pixel-width)1400)))

(defun samray/big-monitor-p ()
  "Detect current sreen whether is it a big monitor or not."
  (or (> (display-pixel-width) 1200)
      (> (display-pixel-height) 1920))
  )
(use-package golden-ratio
  :if (not (samray/big-monitor-p))
  :diminish golden-ratio-mode
  :ensure t
  :init (progn
	  ;; Make `golden-ratio` works with `ace-window`
	  (golden-ratio-mode 1)
	  (add-to-list 'golden-ratio-extra-commands 'ace-window)
	  (setq golden-ratio-auto-scale t)
	  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
	  (add-to-list 'golden-ratio-exclude-modes "dired-mode")
	  (add-to-list 'golden-ratio-exclude-modes "lsp-ui-imenu-mode")
	  (add-to-list 'golden-ratio-exclude-modes "gud-mode")
	  (add-to-list 'golden-ratio-exclude-modes "comint-mode")
	  (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
	  ))

;;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun samray/toggle-window-split ()
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
(when window-system
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
  (mouse-wheel-mode -1))


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
(defun samray/tone-down-fringes ()
  "Set the fringe colors to whatever is the background color."
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))


(defun samray/set-mode-line-attribute ()
  "Set mode line face attribute."
  (interactive)
  (let ((mode-line-height (if (samray/linux-p) 140 155)))
    (dolist (face '(mode-line mode-line-inactive))
      (set-face-attribute face nil
			  :font "Fantasque Sans Mono-15:weight=medium:slant=italic"
			  :height mode-line-height
			  :box nil))))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(add-hook 'after-load-theme-hook #'samray/tone-down-fringes)
(add-hook 'after-load-theme-hook #'samray/set-mode-line-attribute)
;; https://stackoverflow.com/questions/17701576/changing-highlight-line-color-in-emacs
(add-hook 'after-load-theme-hook (lambda ()
				   (set-face-foreground 'hl-line nil)
				   ))

;; no menubar
(menu-bar-mode -1)
;; turn off startup help menu
(setq inhibit-splash-screen t)

;;; Use default line-number-mode instead of nlinum or linum (require Emacs >= 26).
;; (setq-default display-line-numbers-width 1)
(setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode t)

;; number of characters until the fill column
(setq fill-column 80)
;; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)
;; Disable line wrap '\'
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

;;; Disable theme before load a new theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  "Disable theme before load theme."
  (mapc #'disable-theme custom-enabled-themes))
(load-theme 'sanityinc-tomorrow-night t)

(set-frame-font "Fantasque Sans Mono-14:weight=medium:slant=italic")

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

(defun replace-buffer-encoding ()
  "Display the encoding and eol style of the buffer the same way atom does."
  (propertize
   (concat (pcase (coding-system-eol-type buffer-file-coding-system)
	     (0 "LF") 			;LF: Line Feed, 换行符 '\n'
	     (1 "CLF") 		;CLF: Carriage Return + Line Feed, '\r\n'
	     (2 "CR"))			;CR: Carriage Return, 回车 '\r'
	   (let ((sys (coding-system-plist buffer-file-coding-system)))
	     (cond ((memq (plist-get sys :category)
			  '(coding-category-undecided coding-category-utf-8))
		    " UTF-8")
		   (t (upcase (symbol-name (plist-get sys :name))))))
	   " ")))


;;; customize mode line
(setq-default mode-line-format '("%e"
				 mode-line-front-space
				 (:eval (propertize (format "%s" (replace-buffer-encoding))))

				 ;; mode-line-client
				 ;; mode-line-modified -- show buffer change or not
				 ;; mode-line-remote -- no need to indicate this specially
				 ;; mode-line-frame-identification -- this is for text-mode emacs only
				 "["
				 mode-name
				 ":"
				 mode-line-buffer-identification
				 "]"
				 " "
				 mode-line-position
				 (vc-mode vc-mode)
				 " ["
				 (:eval
				  (pcase flycheck-last-status-change
				    (`finished (if flycheck-current-errors
						   (let ((error-count (let-alist (flycheck-count-errors flycheck-current-errors)
									(+ (or .error 0))))
							 (warning-count (let-alist (flycheck-count-errors flycheck-current-errors)
									  (+ (or .warning 0)))))
						     (progn
						       (propertize (concat (when (> error-count 0) (format "✖ %s Error%s "  error-count (if (eq 1 error-count) "" "s")) )
									   (when (> warning-count 0) (format  "⚠ %s Warning%s"warning-count (if (eq 1 warning-count) "" "s") ))))
						       ))
						 (propertize "✔ No Issues")))
				    (`running (propertize "⟲ Running..."))
				    (`no-checker (propertize "⚠ No Checker"))
				    (`not-checked "✖ Disabled")
				    (`errored (propertize "⚠ Error"))
				    (`interrupted "⛔ Interrupted")))
				 "]"
				 ;; mode-line-misc-info
				 ;; mode-line-end-spaces
				 ))
;;; Use Miminish minor modes to change the mode line

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
