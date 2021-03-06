;;; PACKAGE --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:

(defmacro ramsay/after-stack-clears (&rest body)
  "Do BODY after the call stack is clear."
  `(run-with-timer ramsay-idle-time nil (lambda () ,@body)))
(use-package counsel
  :ensure t
  :init ()
  )
(use-package swiper
  :ensure t)

(use-package smex
  :ensure t)

(use-package avy
  :commands (avy-goto-char avy-goto-line)
  :ensure t)

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :diminish ivy-mode
  :config(progn
	   (setq ivy-use-selectable-prompt t)
	   (setq ivy-initial-inputs-alist nil)
	   ;; Use grep instead of grep
	   (setq counsel-grep-base-command
		 "rg -i -M 120 --no-heading --line-number --color never %s %s")
	   ;; Count candidates
	   (setq ivy-count-format "%d/%d ")
	   ;; Number of result lines to display, set height as width-body-height/2
	   (setq ivy-height 25)
	   (defun ramsay/change-ivy-height-dynamicly ()
	     (interactive)
	     (let ((window-split-height (/ (window-body-height)2)))
	       (if (< window-split-height 25)
		   (setq ivy-height 25)
		 (setq ivy-height window-split-height))))
	   )
  )

;;; 支持ivy使用拼音进行匹配
(use-package pyim
  :ensure t
  :config
  (defun pinyin-ivy-cregexp (str)
    (let ((a (ivy--regex-plus str))
          (b (let ((case-fold-search nil))
               (pyim-cregexp-build str))))
      (if (and a (stringp a))
          (concat a "\\|" b)
        a)))

  (setq ivy-re-builders-alist
	'(
	  (counsel-describe-function . ivy--regex-plus)
	  (counsel-describe-variable . ivy--regex-plus)
	  (t . pinyin-ivy-cregexp)
	  ))
  (pyim-isearch-mode t)
  )

(use-package counsel-projectile
  :ensure t
  :after projectile
  :init (counsel-projectile-mode)
  )

(use-package ivy-buffer-extend
  :load-path "~/.emacs.d/additional-packages/ivy-buffer-extend.el")

;;; Steal from https://github.com/alexmurray/ivy-xref
(defun ivy-xref-make-collection (xrefs)
  "Transform XREFS into a collection for display via `ivy-read'."
  (let ((collection nil))
    (dolist (xref xrefs)
      (with-slots (summary location) xref
        (let ((line (xref-location-line location))
              (file (xref-location-group location))
              (candidate nil))
          (setq candidate (concat
                           ;; use file name only
                           (car (reverse (split-string file "\\/")))
			   (when (string= "integer" (type-of line))
			     (concat ":" (int-to-string line) ": "))
			   summary))
          (push `(,candidate . ,location) collection))))
    collection))

(defun ivy-xref-show-xrefs (xrefs alist)
  "Show the list of XREFS and ALIST via ivy."
  (let ((buffer (current-buffer)))
    (ivy-read "xref: " (ivy-xref-make-collection xrefs)
              :require-match t
              :sort t
              :action #'(lambda (candidate)
                          (xref--show-location (cdr candidate) 'quit)))
    ;; honor the contact of xref--show-xref-buffer by returning its original
    ;; return value
    buffer))

(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

;;; Sometimes I find too many buffers is distracted
(defun ramsay/switch-to-current-open-buffer ()
  "Switch to current open bufffer instead of also including;
bookmarks reccently opened files and window layout."
  (interactive)
  (setq ivy-use-virtual-buffers nil)
  (ivy-switch-buffer)
  )

(defun ramsay/ivy-switch-to-buffer-enhanced ()
  "Ivy-switch-to-buffer with recentf."
  (interactive)
  (setq ivy-use-virtual-buffers t)
  (ivy-switch-buffer))


(message "loading init-ivy")
(provide 'init-ivy)

;;; init-ivy.el ends here
