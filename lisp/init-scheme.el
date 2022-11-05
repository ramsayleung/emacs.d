;;; package --- summary
;;; code:
;;; commentary:

(use-package geiser
  :ensure t
  :defer t
  :init
  (setq geiser-active-implementations '(racket))
  (add-hook 'scheme-mode-hook 'geiser-mode)
  )

(message "loading init-scheme")
(provide 'init-scheme)

;;; init-scheme.el ends here
