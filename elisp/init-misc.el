;;; package --- Summary
;;; code:
;;; Commentary:

;; make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t)

;; help you use shell easily on Emacs
(use-package shell-pop
  :ensure t)

;; enhance dired
;; (require 'dired+)
;; (diredp-toggle-find-file-reuse-dir t)

;; dired reuse directory buffer https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))
					; was dired-up-directory
	    ))
(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
	      ;; (filename (dired-get-filename))
	      (filename (dired-get-filename t t))
	      (bye-p (file-directory-p filename)))
	 ad-do-it
	 (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
	   (kill-buffer orig))))))

;; Emacs extension to increate selected region by semantic units
(use-package expand-region
  :ensure t
  :config())

;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
(use-package popwin
  :ensure t
  :config(popwin-mode t))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(delete-selection-mode t)
(setq make-backup-files nil)
;; open init file quickly by binding key
(defun open-my-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; signature
					    ("8sa" "samray")
 					    ))
;; auto indent file before save file
(defun indent-buffer()
  (interactive)
  (indent-region (point-min)(point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region"))
      (progn
	(indent-buffer)
	(message "Indented buffer")))))
(add-hook 'before-save-hook 'indent-region-or-buffer)

;; enable hippie-mode to enhance auto-completion

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol
					 ))


(fset 'yes-or-no-p 'y-or-n-p)

(require 'dired-x)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)

(defun remove-dos-eol ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)(replace-match "")))
;; dwim=do what i mean

(defun occur-dwim()
  "Call `occur` with a sane default"
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))
(set-language-environment "UTF-8")
(setq x-select-enable-clipboard-manager nil)
(defun af-eshell-here ()
  "Go to eshell and set current directory to the buffer's directory."
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name)
                                      default-directory))))
    (if (get-buffer "*eshell*")
	(if (string= (buffer-name) "*eshell*")(delete-window (selected-window))
	  (progn
	    (switch-to-buffer "*eshell*"))
	  )
      (progn
	(split-window-vertically)
	(other-window 1)
	(eshell)
	(eshell/pushd ".")
	(cd dir)
	(goto-char (point-max))
	(eshell-kill-input)
	(eshell-send-input)
	))))
;; (defadvice pop-to-buffer (before cancel-other-window first)
;;   (ad-set-arg 1 nil))

;; (ad-activate 'pop-to-buffer)

;; ;; Toggle window dedication
;; (defun toggle-window-dedicated ()
;;   "Toggle whether the current active window is dedicated or not."
;;   (interactive)
;;   (message
;;    (if (let (window (get-buffer-window (current-buffer)))
;;          (set-window-dedicated-p window 
;;                                  (not (window-dedicated-p window))))
;;        "Window '%s' is dedicated"
;;      "Window '%s' is normal")
;;    (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'toggle-window-dedicated)
;; (add-hook 'eshell-mode-hook
;; 	  (lambda ()
;; 	    (interactive)
;; 	    (set-window-dedicated-p (selected-window) 1)))
(provide 'init-misc)
;;; init-misc.el ends here
