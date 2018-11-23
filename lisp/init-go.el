;;; package --- Summary:
;;; Commentary:
;;; Code:
(defvar samray-default-go-compile-command "go build")
(defvar samray-default-go-run-command "go run")

(defun samray/compile-go-with-command (command)
  "Compile go with COMMAND and run it."
  (let ((file-name (buffer-file-name)))
    (compile (format "%s %s" command file-name))
    )
  )

(defun samray/compile-go (command)
  "Compile go with COMMAND."
  (interactive
   (list (read-string (format "Compile command [default: %s]: " samray-default-go-compile-command) nil nil samray-default-go-compile-command)))
  (samray/compile-go-with-command command)
  )

(defun samray/run-go (command)
  "Run go with COMMAND."
  (interactive
   (list (read-string (format "Run command [default: %s]: " samray-default-go-run-command) nil nil samray-default-go-run-command)))
  (samray/compile-go-with-command command)
  )

(defun samray/golang-src-dir ()
  "Open tmp directory."
  (interactive)
  (dired (concat (getenv "GOPATH") "/src")))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config (progn
	    ;; Use goimports instead of go-fmt
	    (setq gofmt-command "goimports")
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
