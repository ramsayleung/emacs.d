;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config (progn
	          ;; Use goimports instead of go-fmt
	          (setq gofmt-command "goimports")
	          ))

(message "loading init-go")
(provide 'init-go)
;;; init-go.el ends here
