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

(defun ramsay/format-xml (begin end)
  "Pretty format XML markup in region, from BEGIN to END. You need to have nxml-mode.
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Formated xml!"))

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

(defun ramsay/indent-region-or-buffer()
  "Indent selected region or a whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region"))
      (progn
	(ramsay/indent-buffer)
	(message "Indented buffer")))))

;; enable hippie-mode to enhance auto-completion
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol
					 ))

;; from magnars https://github.com/magnars
;;; After Emacs 28 releasing, this can replaced with `rename-buffer`
(defun ramsay/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir (file-name-directory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
		 (recentf-add-file new-name)
		 (recentf-remove-if-non-kept filename))
               (when (and(projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

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
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
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
