;;; package --- Summary:
;;; Commentary:
;;; Code:
(defvar ramsay-default-go-compile-command "go build")
(defvar ramsay-default-go-run-command "go run")

(defun ramsay/compile-go-with-command (command)
  "Compile go with COMMAND and run it."
  (let ((file-name (buffer-file-name)))
    (compile (format "%s %s" command file-name))
    )
  )

(defun ramsay/compile-go (command)
  "Compile go with COMMAND."
  (interactive
   (list (read-string (format "Compile command [default: %s]: " ramsay-default-go-compile-command) nil nil ramsay-default-go-compile-command)))
  (ramsay/compile-go-with-command command)
  )

(defun ramsay/run-go (command)
  "Run go with COMMAND."
  (interactive
   (list (read-string (format "Run command [default: %s]: " ramsay-default-go-run-command) nil nil ramsay-default-go-run-command)))
  (ramsay/compile-go-with-command command)
  )

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
