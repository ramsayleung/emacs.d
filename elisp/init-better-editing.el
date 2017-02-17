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
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
    )
  )

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

;;;  highlight indentation
(use-package highlight-indentation
  :diminish highlight-indentation-mode
  :ensure t
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode)
  :config (progn
	    (set-face-background 'highlight-indentation-face "#b2f3f7")
	    (set-face-background 'highlight-indentation-current-column-face "#b2f3f7")
	    )
  )
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

;;; treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :config (global-undo-tree-mode t))
;;; Emacs always for confirmation whether we really wanna open
;;; large file.However,the default limit is so low,so it prompt often
;;; So increase limit to solve it.
(setq large-file-warning-threshold (* 15 1024 1024)) ;15MB

;;; better default for ediff
(provide 'init-better-editing)
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

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
  "Move region (transient-mark-mode active) or current line 
arg lines down."
  (interactive "*p")
  (samray/move-text-internal arg))
(defun samray/move-text-up (arg)
  "Move region `(transient-mark-mode active) or current line arg lines up by reverse ARG."
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


;;; init-better-editing.el ends here
