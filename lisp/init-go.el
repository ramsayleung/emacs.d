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
  :init (progn
	  (defun samray/buffer-contain-string-p (string)
	    (save-excursion
	      (goto-char (point-min))
	      (search-forward string nil t)))
	  (defvar go-list-index-file-path "/tmp/go-list.index")
	  (defvar go-list-buffer-name "*go-package-go-list*")
	  (defvar go-list-all-warning "go: warning: \"all\" matched no packages")
	  (defun samray/generate-go-list-index ()
	    "Generate go list index by running shell command async"
	    (interactive)
	    (when (get-buffer go-list-buffer-name)
	      (with-current-buffer go-list-buffer-name (erase-buffer)))
	    ;; Avoid poping up the `'go-list-buffer`'
	    (add-to-list 'display-buffer-alist (cons go-list-buffer-name (cons #'display-buffer-no-window nil)))
	    (let* ((proc (progn
			   (async-shell-command "export GO111MODULE=auto;  go list -e all" go-list-buffer-name)
			   (get-buffer-process go-list-buffer-name))))
	      (if (process-live-p proc)
		  (set-process-sentinel proc #'samray/after-go-list-index)
		(message "No process running."))))

	  (defun samray/after-go-list-index (process signal)
	    "After generate go-list-index, kill buffer and save index to file."
	    (when (memq (process-status process) '(exit signal))
	      (message "Running go list successfully!")
	      (with-current-buffer go-list-buffer-name
		(progn
		  (when (and (not (samray/buffer-contain-string-p go-list-all-warning)))
		    (delete-file go-list-index-file-path)
		    (append-to-file (point-min) (point-max) go-list-index-file-path))
		  ;; (kill-buffer go-list-buffer-name)
		  ))
	      (shell-command-sentinel process signal)))

	  (defun samray/process-go-list-index ()
	    "Process go list index file"
	    (with-temp-buffer
	      (insert-file-contents go-list-index-file-path)
	      (progn
		(goto-char (point-min))
		(let ((lines)
		      (current-line)
		      (go-list-process-name "Process"))
		  (while (not (eobp))
		    (setq current-line (buffer-substring-no-properties
					(line-beginning-position)
					(line-end-position)))
		    (when (not (string-match-p go-list-process-name current-line))
		      (setq lines (cons current-line lines)))
		    (forward-line 1))
		  (nreverse lines))))))
  :config (progn
	    ;; Use goimports instead of go-fmt
	    (setq gofmt-command "goimports")
	    (setq go-packages-function 'samray/process-go-list-index)

	    ;; (completing-read "Package: " (samray/process-go-list-index))
	    (add-hook 'after-init-hook 'samray/generate-go-list-index)
	    ;; regenerate go list index every 10 min;
	    (run-with-timer 60 (* 30 60) 'samray/generate-go-list-index)
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
