;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package magit
  :commands (magit-stage)
  :ensure t
  )

;;; like a time machine,to move to past versions of the current file
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine-toggle
	     git-timemachine-switch-toggle))
(provide 'init-version-control)
;;; init-version-control.el ends here
