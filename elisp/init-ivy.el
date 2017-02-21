;;; package --- Summary
;;; code:
;;; Commentary:
(use-package counsel
  :ensure t
  )
(use-package swiper
  :ensure t)
(use-package avy
  :commands (avy-goto-char avy-goto-line)
  :ensure t)

(use-package ivy :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  (defun samray/counsel-goto-recent-directory ()
    "Open recent directory with dired"
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
	   (delete-dups
	    (append (mapcar 'file-name-directory recentf-list)
		    ;; fasd history
		    (if (executable-find "fasd")
			(split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "directories:" collection :action 'dired)))
  )
(use-package counsel-projectile
  :ensure t
  :config(counsel-projectile-on))
(provide 'init-ivy)
;;; init-ivy.el ends here
