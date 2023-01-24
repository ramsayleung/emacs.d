;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package magit
  :commands (magit-stage magit-status)
  :ensure t
  :init (progn
	  ;;; Force redisplay git branch of the mode line.
	  (setq auto-revert-check-vc-info t)

	  ;; Disable version control checking for Tramp.
	  (setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))
	  )
  )

(use-package smerge-mode
  :ensure t
  :config
  (defun enable-smerge-maybe ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode +1)))))

  (add-hook 'buffer-list-update-hook #'enable-smerge-maybe)
  )

(message "loading init-version-control")
(provide 'init-version-control)

;;; init-version-control.el ends here
