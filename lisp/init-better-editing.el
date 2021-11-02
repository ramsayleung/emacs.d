;;; package --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:

(use-package ace-window
  :ensure t
  :commands (ace-window)
  )

;; Delete spaces at once
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (progn
	  (setq which-key-idle-delay 0.3)
	  (setq which-key-enable-extended-define-key t)
	  )
  :config(progn
	   (which-key-mode t)
	   ))

;;; Highlight delimiter such as parenthese,brackets or braces
;;; according to their depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; Emacs minor mode to highlight indentation
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :config (progn
	    (setq highlight-indent-guides-method 'character)
	    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
  )

;;; show information about selected region
(use-package region-state
  :ensure t
  :config (region-state-mode t))

;;; folding based on indentation/syntax
(use-package origami
  :if (not (eq system-type 'windows-nt))
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

;;; make grep buffer writable and apply the changes to files
(use-package wgrep
  :ensure t
  :config (progn
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
 	    (defun ramsay/popup-which-function ()
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
(run-with-idle-timer ramsay-idle-time t 'recentf-mode)
;; (recentf-mode 1)

;;; Auto-refresh buffers when files have changed on disk
(global-auto-revert-mode t)

(defun ramsay/split-window-right-and-move ()
  "Split window vertically and move to the other window."
  (interactive)
  (split-window-right)
  (other-window 1)
  )

(defun ramsay/split-window-below-and-move ()
  "Split window horizontally and move to the other window!"
  (interactive)
  (split-window-below)
  (other-window 1)
  )

;;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun ramsay/smarter-move-beginning-of-line (arg)
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

(defun ramsay/copy-current-file-path ()
  "Add current file path to kill ring.  Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))


;;; http://blog.binchen.org/posts/the-reliable-way-to-access-system-clipboard-from-emacs.html
;;; Copy and Paste in x system in all platform
(use-package simpleclip
  :ensure t
  :config(progn
	   (defun ramsay/copy-to-x-clipboard ()
	     (interactive)
	     (let ((thing (if (region-active-p)
			      (buffer-substring-no-properties (region-beginning) (region-end))
			    (thing-at-point 'symbol))))
	       (simpleclip-set-contents thing)
	       (message "thing => clipboard!")))

	   (defun ramsay/paste-from-x-clipboard()
	     "Paste string clipboard"
	     (interactive)
	     (insert (simpleclip-get-contents)))
	   ))

(setq dired-dwim-target t)
(message "loading init-better-editing")
(provide 'init-better-editing)
;;; init-better-editing.el ends here
