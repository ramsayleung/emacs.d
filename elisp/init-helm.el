;;; package --- Summary
;;; Code:
;;; Commentary:
(use-package helm)
(use-package helm-ag
  :config
  (evil-leader/set-key
    "s a a" 'helm-ag-this-file
    "s a f" 'helm-do-ag
    "s a p" 'helm-project-do-ag)
  )

(provide 'init-helm)
;;; init-helm.el ends here
