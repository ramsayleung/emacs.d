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
	   )
  )

;;; 支持ivy使用拼音进行匹配
(use-package pyim
  :ensure t
  :diminish pyim-isearch-mode
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
  :if (not emacs/>=28p)
  :ensure t
  :after projectile
  :init (counsel-projectile-mode)
  )

(use-package ivy-buffer-extend
  :load-path "~/.emacs.d/additional-packages/ivy-buffer-extend.el")

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
