;; package --- Summary
;;; code:
;;; Commentary:

(use-package ledger-mode
  :ensure t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :custom (ledger-clear-whole-transactions t)
  :config
  (use-package flycheck-ledger
    :ensure t
    :after ledger-mode)
  )

;;; Try out Emacs Package without install
(use-package try
  :commands try
  :ensure t)

;;; abbrev-mode or abbreviation mode is a built-in mode that auto-corrects the
;;; word you mistype on pressing space.For how I practically use it
(setq abbrev-mode 'silently)
(setq save-abbrevs t)

;; Only start server mode if I'm not root
(require 'server)
(unless (server-running-p) (server-start))

(defadvice bookmark-jump (after bookmark-jump activate)
  "Find frequently used bookmarks easily."
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;;; disable `ad-handle-definition: ‘bookmark-jump’ got redefined` warning.
(setq ad-redefinition-action 'accept)

(defun ramsay/empty-buffer ()
  "Open a new clean window with a buffer named untitled<N>.
The buffer is not associated with a file."
  (interactive)
  (switch-to-buffer-other-window (generate-new-buffer "untitled")))

;; auto indent file before save file
(defun ramsay/indent-buffer()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min)(point-max)))

(defun ramsay/kill-other-buffers ()
  "Kill all other buffers but current one and *scratch*."
  (interactive)
  (mapc 'kill-buffer
	(delq "*scratch*"(delq (current-buffer) (buffer-list)))))

(defun ramsay/format-with-fallback ()
  "Format buffer using mode-specific formatters with fallback.
For Rust, uses rust-format-buffer
For Go, uses gofmt
Otherwise tries eglot-format, then falls back to ramsay/indent-region-or-buffer"
  (interactive)
  (cond
   ;; Rust mode
   ((eq major-mode 'rust-mode)
    (call-interactively 'rust-format-buffer))
   
   ;; Go mode
   ((eq major-mode 'go-mode)
    (call-interactively 'gofmt))
   
   ;; Other modes - try eglot first, then fallback
   (t
    (condition-case err
        (call-interactively 'eglot-format)
      (error
       (message "eglot-format failed, falling back to indent: %s" (error-message-string err))
       (call-interactively 'ramsay/indent-buffer))))))

;; from magnars
(defun ramsay/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; from magnars
(defun ramsay/sudo-edit (&optional arg)
  "Edit as root with ARG."
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun ramsay/dired-tmp-dir ()
  "Open tmp directory."
  (interactive)
  (if (eq system-type 'windows-nt)
      (dired (getenv "TMPDIR"))
    (dired "/tmp")
    ))

(message "loading init-misc")
(provide 'init-misc)
;;; init-misc.el ends here
