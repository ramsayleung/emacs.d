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

(defun ramsay/kill-other-buffers ()
  "Kill all other buffers but current one and *scratch*."
  (interactive)
  (mapc 'kill-buffer
	    (delq "*scratch*"(delq (current-buffer) (buffer-list)))))

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

(defun ramsay/indent-region-or-buffer()
  "Indent selected region or a whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	    (progn
	      (indent-region (region-beginning) (region-end))
	      (message "Indented selected region"))
      (progn
	    (indent-region (point-min) (point-max))
	    (message "Indented buffer")))))
(message "loading init-misc")
(provide 'init-misc)
;;; init-misc.el ends here
