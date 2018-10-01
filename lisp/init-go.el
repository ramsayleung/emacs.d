;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config (progn
			;; Use goimports instead of go-fmt
			;; (setq gofmt-command "goimports")
			;; Customize compile command to run go build
			(if (not (string-match "go" compile-command))
				(set (make-local-variable 'compile-command)
					 "go generate && go build -v && go test -v && go vet"))
			;; Go oracle
			(let ((oracle-path "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))
			  (when (file-exists-p oracle-path)
				(message "go oracle exists")
				(load-file oracle-path)
				)
			  )
			;; Godef jump key binding
			(local-set-key (kbd "M-.") 'godef-jump)
			(local-set-key (kbd "M-*") 'pop-tag-mark)
			(add-hook 'go-mode-hook
					  (lambda ()
						(setq-default)
						(setq tab-width 2)
						(setq standard-indent 2)
						(setq indent-tabs-mode nil)))
			))

(use-package go-eldoc
  :ensure t
  :defer t
  :config (progn
			(add-hook 'go-mode-hook 'go-eldoc-setup)
			))

(message "loading init-go")
(provide 'init-go)
;;; init-go.el ends here
