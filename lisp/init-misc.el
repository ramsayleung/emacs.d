;;; package --- Summary
;;; code:
;;; Commentary:
;;; read pdf file in Emacs

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init (add-hook 'pdf-view-mode (lambda () (company-mode nil)))
  :ensure t)

;;; Track Emacs commands frequency
(use-package keyfreq
  :ensure t
  :config (keyfreq-mode 1) (keyfreq-autosave-mode 1))

;;; make Emacs sound like a proper typewrite
(use-package selectric-mode
  :commands selectric-mode
  :ensure t)

;;; use Irc in Emacs
(use-package circe
  :ensure t
  :commands circe-mode
  :config (progn
	    (setq circe-network-options
		  '(("Freenode"
		     :tls t
		     :nick "SamrayL"
		     :sasl-username "SamrayL"
		     :sasl-password "123456"
		     :channels ("#emacs")
		     )))
	    ))

;;; Try out Emacs Package without install
(use-package try
  :commands try
  :ensure t)

;;; Defining and querying search engine(for example:Google) through Emacs
(use-package  engine-mode
  :ensure t
  :diminish
  :config (progn
	    (engine-mode t)
	    (defengine amazon
	      "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

	    (defengine duckduckgo
	      "https://duckduckgo.com/?q=%s"
	      :keybinding "d")

	    (defengine github
	      "https://github.com/search?ref=simplesearch&q=%s"
	      :keybinding "h")

	    (defengine google
	      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
	      :keybinding "g")

	    (defengine google-images
	      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

	    (defengine google-maps
	      "http://maps.google.com/maps?q=%s"
	      :docstring "Mappin' it up.")

	    (defengine project-gutenberg
 	      "http://www.gutenberg.org/ebooks/search/?query=%s")

	    (defengine rfcs
	      "http://pretty-rfc.herokuapp.com/search?q=%s")

	    (defengine stack-overflow
	      "https://stackoverflow.com/search?q=%s")

	    (defengine twitter
	      "https://twitter.com/search?q=%s")

	    (defengine wikipedia
	      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
	      :keybinding "w"
	      :docstring "Searchin' the wikis.")

	    (defengine wiktionary
	      "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

	    (defengine wolfram-alpha
	      "http://www.wolframalpha.com/input/?i=%s")

	    (defengine youtube
	      "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
	    ))

(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

;;; abbrev-mode or abbreviation mode is a built-in mode that auto-corrects the
;;; word you mistype on pressing space.For how I practically use it
(setq abbrev-mode 'silently)
(setq save-abbrevs t)

(defadvice bookmark-jump (after bookmark-jump activate)
  "Find frequently used bookmarks easily."
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

(define-abbrev-table 'global-abbrev-table '(
					    ;; signature
					    ("8sa" "samray")
 					    ))

;;; remove blank line in buffer
(defun samray/delete-blank-line-in-buffer ()
  "As function definition."
  (interactive)
  (flush-lines "^$"))

;; auto indent file before save file
(defun samray/indent-buffer()
  (interactive)
  (indent-region (point-min)(point-max)))

(defun samray/indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region"))
      (progn
	(indent-buffer)
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


;;; remove windows end-of-line delimiter
(defun remove-dos-eol ()
  "Remove dos/windows eol."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)(replace-match "")))

;; dwim=do what i mean
(defun occur-dwim()
  "Call `occur` with a sane default"
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))

(defun samray/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the;
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

;; from magnars https://github.com/magnars
(defun samray/rename-current-buffer-file ()
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

(defun samray/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (when (and (configuration-layer/package-usedp 'projectile)
                 (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache)))))

;; from magnars
(defun samray/delete-current-buffer-file ()
  "Remove file connected to current buffer and kills buffer."
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
(defun samray/sudo-edit (&optional arg)
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

(defun samray/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;;; http://blog.binchen.org/posts/open-readme-under-git-root-directory-in-emacs.html
(defun samray/open-readme-in-git-root-directory ()
  "Open README file at the root directory of my project."
  (interactive)
  (let (filename
        (root-dir (locate-dominating-file (file-name-as-directory (file-name-directory buffer-file-name)) ".git"))
        )
    ;; (message "root-dir=%s" root-dir)
    (and root-dir (file-name-as-directory root-dir))
    (setq filename (concat root-dir "README.org"))
    (if (not (file-exists-p filename))
        (setq filename (concat root-dir "README.md"))
      )
    ;; (message "filename=%s" filename)
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename nil nil))
      (message "NO README.org or README.md found!"))
    ))


(message "loading init-misc")
(provide 'init-misc)
;;; init-misc.el ends here
