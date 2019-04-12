					; PACKAGE --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:

(defmacro samray/after-stack-clears (&rest body)
  "Do BODY after the call stack is clear."
  `(run-with-timer 1 nil (lambda () ,@body)))

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
  :diminish ivy-mode
  :init (progn
	  (setq ivy-use-selectable-prompt t)
	  (setq ivy-re-builders-alist
		'((t . ivy--regex-plus)))
	  (setq ivy-initial-inputs-alist nil)
	  )
  :config(progn
	   (ivy-mode 1)
	   ;; Count candidates
	   (setq ivy-count-format "%d/%d ")
	   ;; Number of result lines to display, set height as width-body-height/2
	   (defun samray/change-ivy-height-dynamicly ()
	     (interactive)
	     (setq ivy-height (/ (window-body-height)2)))
	   ;; Set `ivy-height` after Emacs startup.
	   (run-with-idle-timer 0.5 nil 'samray/change-ivy-height-dynamicly)
	   (defun samray/counsel-grep-or-swiper ()
	     "Use `counsel-grep-or-swiper` dependen on buffer size."
	     (interactive)
	     (if (samray/buffer-too-big-p)
		 (isearch-forward)
	       (counsel-grep-or-swiper)))
	   )
  )

(use-package ivy-posframe
  :defer t
  :ensure t
  :defer t
  :config (progn
	    (run-with-idle-timer 0.1 nil 'ivy-posframe-enable)
	    ;; (setq ivy-display-function #'ivy-posframe-display)
	    ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
	    ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
	    ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
	    (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
	    ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
	    (setq ivy-posframe-font "Fantasque Sans Mono-14:weight=medium:slant=italic")
	    ;; The behaviors of Ivy posframe is different between mac and linux
	    (defun samray/setup-ivy-window()
	      "Set up ivy height and width."
	      (setq ivy-posframe-min-height (/(window-body-height)2))
	      (setq ivy-posframe-height (/ (window-body-height)2))
	      (setq ivy-posframe-width (window-body-width))
	      (setq ivy-posframe-min-width (window-body-width))
	      )
	    (samray/setup-ivy-window)
	    (defadvice select-window (after select-window-ivy activate)
	      "Advice `select-window` to set up ivy-posframe-width/height dynamicly."
	      (samray/setup-ivy-window))))

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
        '((t . pinyin-ivy-cregexp))))

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
(defun samray/switch-to-current-open-buffer ()
  "Switch to current open bufffer instead of also including;
bookmarks reccently opened files and window layout."
  (interactive)
  (setq ivy-use-virtual-buffers nil)
  (ivy-switch-buffer)
  )

(defun samray/ivy-switch-to-buffer-enhanced ()
  "Ivy-switch-to-buffer with recentf."
  (interactive)
  (setq ivy-use-virtual-buffers t)
  (ivy-switch-buffer))

;; http://emacs.stackexchange.com/questions/10393/how-can-i-answer-a-minibuffer-prompt-from-elisp
(defun samray/insert-symbol-at-point ()
  (if (> (length cached-symbol-at-point) 0)
      (insert cached-symbol-at-point))
  (remove-hook 'post-command-hook 'insert-symbol-at-point)
  )

(defun samray/counsel-ag-symbol-at-point ()
  "Search for number at point using counsel-ag."
  (interactive)
  (setq cached-symbol-at-point (thing-at-point `symbol))
  (add-hook 'post-command-hook 'insert-symbol-at-point)
  (counsel-ag)
  )

(message "loading init-ivy")
(provide 'init-ivy)

;;; init-ivy.el ends here
