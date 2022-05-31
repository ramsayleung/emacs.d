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

(message "loading init-version-control")
(provide 'init-version-control)

;;; init-version-control.el ends here
