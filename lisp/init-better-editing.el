;;; package --- Summary
;;; code:
;;; Commentary:

(use-package window-numbering
  :ensure t
  :config(progn
	   (window-numbering-mode t)))

(use-package smartparens
  :ensure t
  :demand t
  :diminish (smartparens-mode . "ρ")
  :config
  (progn
    (smartparens-global-mode t)
    ;; show single quote "'" in emacs and lisp-interaction-mode instead of single quote pair "''"
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode rust-mode) "'" nil :actions nil)
    )
  )

(require 'cl)
(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")
            (angle-bracket . "<")))
;; delete spaces at once
(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode t))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config(progn
	   (which-key-mode t)
	   (setq which-key-idle-delay 0.3)
	   ))

;;; Highlight delimiter such as parenthese,brackets or braces
;;; according to their depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
;;; A light that follows your cursor around so you don't lose it
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :demand t
  :config
  (beacon-mode t))

;;; An useful package to compare directory tree
(use-package ztree
  :ensure t
  :commands (ztree-dir ztree-diff)
  :init (setq ztree-dir-move-focus t))

;;; Emacs minor mode to highlight indentation
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :defer t
;;   :init (progn
;;           (setq highlight-indent-guides-method 'character)
;;           (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
;;   )

;;; show information about selected region
(use-package region-state
  :ensure t
  :config (region-state-mode t))

;;; folding based on indentation/syntax
(use-package origami
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'prog-mode-hook (lambda () (origami-mode t)))
	  ))

;;; Show current buffer's imenu entries in a seperate buffer
(use-package imenu-list
  :ensure t
  :commands imenu-list-smart-toggle
  :init (progn
	  (setq imenu-list-focus-after-activation t)
	  (setq imenu-list-auto-resize t)
	  (setq imenu-list-position 'right)
	  (setq imenu-list-size 0.3)
	  (add-hook 'imenu-list-major-mode-hook
		    (lambda () (turn-off-evil-mode)))
	  ))

;;; treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :config (global-undo-tree-mode t))

;;; make grep buffer writable and apply the changes to files
(use-package wgrep
  :ensure t
  :init (progn
	  (setq wgrep-auto-save-buffer t)
	  ))

;;; Emacs always for confirmation whether we really wanna open
;;; large file.However,the default limit is so low,so it prompt often
;;; So increase limit to solve it.
(setq large-file-warning-threshold (* 15 1024 1024)) ;15MB

;;; Visual Popup Interface Library For Emacs
(use-package popup
  :ensure t
  :config (progn
 	    (defun samray/popup-which-function ()
	      (interactive)
	      (let ((function-name (which-function)))
		(popup-tip function-name)))))

;;;Winner mode is an Emacs built-in package that lets you undo and redo window
;;;configurations. Incredibly useful since I keep splitting and merging windows
;;;https://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode t))

;;; Enable narrow to region feature if it is disable
(put 'narrow-to-region 'disabled nil)

(require 'recentf)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))
(recentf-mode 1)

;;; Auto-refresh buffers when files have changed on disk
(global-auto-revert-mode t)

(defun samray/split-window-right-and-move ()
  "Split window vertically and move to the other window."
  (interactive)
  (split-window-right)
  (other-window 1)
  )

(defun samray/split-window-below-and-move ()
  "Split window horizontally and move to the other window!"
  (interactive)
  (split-window-below)
  (other-window 1)
  )

;;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun samray/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun samray/query-replace-dwim (replace-string)
  "Enhance query and replace."
  (interactive
   (list (read-string (format "Do query-replace %s with :" (thing-at-point-or-at-region)))))
  (save-excursion
    (let ((replaced-string (thing-at-point-or-at-region)))
      (goto-char (point-min))
      (query-replace replaced-string replace-string))))

(defun thing-at-point-or-at-region ()
  "Return string or word at the cursor or in the marked region."
  (if (region-active-p)
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
    (thing-at-point 'symbol)))

;;;Code from http://pragmaticemacs.com/emacs/aligning-text/
;;; I change something
(defun samray/align-whitespace (start end)
  "Align columns by whitespace in region between START to END."
  (interactive "r")
  (save-excursion
    (if (region-active-p)
	(progn
	  (align-regexp start end
			"\\(\\s-*\\)\\s-" 1 0 t)
	  (message "Align columns done"))
      (message "You have to mark some region to align column"))))


(defun samray/copy-current-file-path ()
  "Add current file path to kill ring.  Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))

(defun samray/copy-to-end-of-line ()
  "Replace vim keybinding `y$`."
  (interactive)
  (kill-ring-save (point)
                  (line-end-position)))

;;; Move line up or down and also a region if the region is selected
;; https://www.emacswiki.org/emacs/move-text.el
(defun samray/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun samray/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG
lines down."
  (interactive "*p")
  (samray/move-text-internal arg))

(defun samray/move-text-up (arg)
  "Move region `(transient-mark-mode active) or current line arg
lines up by reverse ARG."
  (interactive "*p")
  (samray/move-text-internal (- arg)))

;;; http://blog.binchen.org/posts/the-reliable-way-to-access-system-clipboard-from-emacs.html
;;; Copy and Paste in x system in all platform
(use-package simpleclip
  :ensure t
  :config(progn
	   (defun samray/copy-to-x-clipboard ()
	     (interactive)
	     (let ((thing (if (region-active-p)
			      (buffer-substring-no-properties (region-beginning) (region-end))
			    (thing-at-point 'symbol))))
	       (simpleclip-set-contents thing)
	       (message "thing => clipboard!")))

	   (defun samray/paste-from-x-clipboard()
	     "Paste string clipboard"
	     (interactive)
	     (insert (simpleclip-get-contents)))	   
	   ))

;;; Changing Ediff options
;;; use window instead of weird frame
;;; https://oremacs.com/2015/01/17/setting-up-ediff/

(require 'ediff)
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)

;;; changing ediff key binding
(defun samray/ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'samray/ediff-hook)

(defvar samray/ediff-last-windows nil)

(defun samray/store-pre-ediff-winconfig ()
  (setq samray/ediff-last-windows (current-window-configuration)))

(defun samray/restore-pre-ediff-winconfig ()
  (set-window-configuration samray/ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'samray/store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'samray/restore-pre-ediff-winconfig)

;; -*- lexical-binding: t -*-
(defun samray/ediff-files ()
  "Ediff in 'dired-mode'."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))
(require 'dired)
(define-key dired-mode-map "e" 'samray/ediff-files)

(defadvice upcase-word (before upcase-word-advice activate)
  "Upcase a word until cursor is at the beginning of this word."
  (unless (looking-back "\\b" 1)(backward-word)))

(defadvice downcase-word (before downcase-word-advice activate)
  "Downcase a word until cursor is at the beginning of this word."
  (unless (looking-back "\\b" 1)(backward-word)))

(defadvice capitalize-word (before capitalize-word-advice activate)
  "Capitalize word dependen on 'subword-mode'."
  (unless (or (looking-back "\\b" 1)
              (bound-and-true-p subword-mode))
    (backward-word)))

(provide 'init-better-editing)
;;; init-better-editing.el ends here
