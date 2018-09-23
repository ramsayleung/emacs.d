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

;; language
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
	    (push '("*Youdao Dictionary*" :noselect t :width 0.2 :position bottom) popwin:special-display-config)
	    ))

(defun samray/toggle-golden-ratio()
  "Check whether turn on golden ratio."
  (or (< (display-pixel-height)800)
      (< (display-pixel-width)1400)))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :ensure t
  :defer t
  :init (progn
	  (golden-ratio-mode 1)
	  (setq golden-ratio-auto-scale t)
	  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
	  (add-to-list 'golden-ratio-exclude-modes "lsp-ui-imenu-mode")
	  (add-to-list 'golden-ratio-exclude-modes "gud-mode")
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
  (mouse-wheel-mode -1)
  )


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


(defun samray/set-mode-line-width ()
  "Set mode line width, it is so cool."
  (set-face-attribute 'mode-line nil
		      :box '())
  )
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(add-hook 'after-load-theme-hook 'samray/tone-down-fringes)
(add-hook 'after-load-theme-hook #'samray/set-mode-line-width)
;; https://stackoverflow.com/questions/17701576/changing-highlight-line-color-in-emacs
(add-hook 'after-load-theme-hook (lambda ()
				   (set-face-foreground 'hl-line nil)
				   ))

;; no menubar
(menu-bar-mode -1)
;; turn off startup help menu
(setq inhibit-splash-screen t)

;;; Use default line-number-mode instead of nlinum of linum (require Emacs >= 26).
(setq-default display-line-numbers-width 2)
(setq display-line-numbers-current-absolute t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; number of characters until the fill column
(setq fill-column 80)
;; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)
;;; make sure my code stays within 100 characters always
;;; and prefer the soft line wrap while writing prose
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
(global-hl-line-mode 1)
;; don't blink the cursor
(blink-cursor-mode -1)

;; Only use `bar' type of cursor shape
(add-hook 'minibuffer-setup-hook '(lambda () (setq cursor-type 'bar)))

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
  :defer t
  )
;;; Disable theme before load a new theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  "Disable theme before load theme."
  (mapc #'disable-theme custom-enabled-themes))

(defvar samray-current-font nil)
(defun samray/reset-current-font (&rest args)
  "It seems a bug about EMACS that the font will change after
load/'disable-theme', so reset it after load/disable-theme' ARGS"
  (set-frame-font samray-current-font)
  )

(advice-add 'disable-theme :after 'samray/reset-current-font)
;; Cycle through this set of themes
(defvar samray-theme-list '(zenburn manoj-dark))

(defvar samray-current-theme nil)
(defun samray/cycle-theme ()
  "Cycle through a list of themes, samray-theme-list."
  (interactive)
  (when samray-current-theme
    (setq samray-theme-list (append samray-theme-list (list samray-current-theme))))
  (setq samray-current-theme (pop samray-theme-list))
  (load-theme  samray-current-theme t)
  )

;; Switch to the first theme in the list above
(add-hook 'after-init-hook #'samray/cycle-theme)
(add-hook 'after-init-hook #'samray/cycle-font)

;; ====================================Themes automatically change =====================================
;;timer for automatically changing themes
(defvar samray--interval-timer nil)

;;table is used to save (time themes) pair for automatically changing themes
;;time should be a string. themes should be a variant , not symbos.
(defvar samray--time-themes-table nil)

(defun samray/config-time-themes-table (theme-table)
  "Set time . THEME-TABLE for time-themes-table."
  (setq samray--time-themes-table
	;; sort firstly, get-themes-according require a sorted table.
	(sort theme-table (lambda (x y) (< (string-to-number (car x)) (string-to-number (car y)))))
        )
  )

(defun samray/get-themes-according (hour-string)
  "This function return the theme according to HOUR-STRING;
Value of hour-string should be between 1 and 24(including)."
  (catch 'break
    (let (
          (now-time (string-to-number hour-string))
          ;; init current-themes to the themes of final item
          (correct-themes (cdr (car (last samray--time-themes-table))))
          (loop-list samray--time-themes-table)
          )

      ;; loop to set correct themes to correct-themes
      (while loop-list
	(let ((v (car loop-list)))
	  (let ((v-time (string-to-number (car v))) (v-themes (cdr v)))
	    (if (< now-time v-time)
                (throw 'break correct-themes)  ; t
	      (setq correct-themes v-themes) ; nil
	      )))
	(setq loop-list (cdr loop-list))
        )
      ;; This is returned for value of hour-string is bigger than or equal to car of final item
      (throw 'break correct-themes) ; t
      ))
  )

(defun samray/check-time-and-modify-theme ()
  "This function will get the theme of now according to time-table-themes;
then check whether EMACS should to modify theme, if so, modify it."
  (let ((new-theme (samray/get-themes-according (format-time-string "%H"))))
    (unless (eq new-theme samray-current-theme)
      (setq samray-current-theme new-theme)
      (load-theme new-theme t)
      ))
  )

(defun samray/open-themes-auto-change ()
  "Start to automatically change themes."
  (interactive)
  (samray/check-time-and-modify-theme)
  (setq
   samray--interval-timer (run-at-time 1800 3600 'samray/check-time-and-modify-theme))
  (message "themes auto change open.")
  )

(defun samray/close-themes-auto-change ()
  "Stop automatically changing themes."
  (interactive)
  (cancel-timer samray--interval-timer)
  (message "themes auto change close.")
  )

;; Usage
;; item of time-themes-table: ( hours-in-string . theme-name)
;; 6:00 - 17::00 use spacemacs-light, 17:00 - 24:00 use monokai, 24:00 - 6:00 use spacemacs-light
;; you could add more items.
(samray/config-time-themes-table '(("6" . zenburn) ("18" . manoj-dark) ))
;; (samray/open-themes-auto-change)
;;---------------;;
;;      Font     ;;
;;---------------;;

(cond ((eq system-type 'gnu/linux)
       (defvar samray-font-list '("Fantasque Sans Mono-12:weight=medium:slant=italic" )))
      ((eq system-type 'darwin)
       (defvar samray-font-list '("Fantasque Sans Mono-12:weight=medium:slant=italic" )))
      ((eq system-type 'windows-nt)
       (defvar samray-font-list '("Consolas-13"))))
(set-frame-font "Fantasque Sans Mono-12:weight=medium:slant=italic")

(defun samray/font-exists-p (font)
  "Check if FONT exists."
  (member font (font-family-list)))

(defun samray/cycle-font ()
  "Cycle through a list of fonts,samray-font-list."
  (interactive)
  (when samray-current-font
    (setq samray-font-list (append samray-font-list (list samray-current-font))))
  (let ((current-font (pop samray-font-list)))
    ;; (when (not (samray/font-exists-p current-font))
    (setq current-font (pop samray-font-list))
    ;; )
    (setq samray-current-font current-font)
    )
  )

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
;;; Force redisplay git branch of the mode line.
(setq auto-revert-check-vc-info t)
;;; customize mode line
(setq-default mode-line-format '("["
				 "%e"
				 (:eval
				  (window-numbering-get-number-string))
				 "]"
				 mode-line-front-space
				 mode-line-mule-info

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
				 " "
				 ;; mode-line-modes -- move major-name above
				 "["
				 minor-mode-alist
				 "]"
				 mode-line-misc-info
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

(provide 'init-ui)

;;; init-ui.el ends here
