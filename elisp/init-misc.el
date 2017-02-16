;;; package --- Summary
;;; code:
;;; Commentary:
;;; read pdf file in Emacs
(use-package pdf-tools
  :mode ("\\.pdf\\'")
  :ensure t)

;;; Make Emacs sound like a proper typewrite
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
	    (evil-leader/set-key
	     "o c" 'circe)
	    ))
;;; Try out Emacs Package without install
(use-package try
  :commands try
  :ensure t)

;;; Get weather status
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("GuangZhou"
                                "ZhaoQing")))

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
;;;enhance dired
(use-package dired+
  :commands dired
  :config
  (diredp-toggle-find-file-reuse-dir t)
  )
(setq dired-open-extensions
      '(("mkv" . "vlc")
        ("mp4" . "vlc")
        ("avi" . "vlc")))
;;; File encoding system
;;; UTF-8 works for most of the files i tend to used
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)



;;; code  from spacemacs
(use-package restart-emacs
  :ensure t
  :commands (samray/restart-emacs
	     samray/restart-emacs-debug-init
	     samray/restart-emacs-resume-layout)
  :init
  (defun samray/restart-emacs (&optional args)
    "Restart emacs."
    (interactive)
    ;; (setq spacemacs-really-kill-emacs t)
    (restart-emacs args))
  (defun samray/restart-emacs-resume-layouts (&optional args)
    "Restart emacs and resume layouts."
    (interactive)
    (samray/restart-emacs (cons "--resume-layouts" args)))
  (defun samray/restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (samray/restart-emacs (cons "--debug-init" args)))
  (defun samray/restart-stock-emacs-with-packages (packages &optional args)
    "Restart emacs without the spacemacs configuration, enable
debug-init and load the given list of packages."
    (interactive
     (progn
       (unless package--initialized
	 (package-initialize t))
       (let ((packages (append (mapcar 'car package-alist)
			       (mapcar 'car package-archive-contents)
			       (mapcar 'car package--builtins))))
	 (setq packages (mapcar 'symbol-name packages))
	 (let ((val (completing-read-multiple "Packages to load (comma separated): "
					      packages nil t)))
	   `(,val)))))
    (let ((load-packages-string (mapconcat (lambda (pkg) (format "(use-package %s)" pkg))
					   packages " ")))
      (samray/restart-emacs-debug-init
       (append (list "-q" "--execute"
		     (concat "(progn (package-initialize) "
			     "(require 'use-package)"
			     load-packages-string ")"))
	       args))))
  )

;;; record how much time i use to programming with wakatimw
(use-package wakatime-mode
  :ensure t
  :diminish (wakatime-mode . " ω")
  :config (message "loading wakatime") (global-wakatime-mode))

(set-language-environment "UTF-8")
(setq x-select-enable-clipboard-manager nil)

;;; Emacs takes regular backups of once you switch on auto-saving
;;; and by default,put backups in the same directory,honestly,sometimes
;;; it is essential useful,but most of time the backups in the same directory
;;; is annoying,there is a good solution from Emacswiki
;;; https://www.emacswiki.org/emacs/BackupDirectory
;; Backups at .saves folder in the current folder
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;;; Emacs auto saves ofter,but it always messes up my file tree.
;;; So,let's Emacs to store its auto-save file in temporary directory
;;; https://www.emacswiki.org/emacs/BackupDirectory
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles nil)

(delete-selection-mode t)
;; open init file quickly by binding key
(defun open-my-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


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

(define-abbrev-table 'global-abbrev-table '(
					    ;; signature
					    ("8sa" "samray")
 					    ))
;; auto indent file before save file
(defun indent-buffer()
  (interactive)
  (indent-region (point-min)(point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region"))
      (progn
	(indent-buffer)
	(message "Indented buffer")))))
(add-hook 'prog-mode-hook (lambda ()
			    (unless (derived-mode-p '(python-mode ))
			      (add-hook 'before-save-hook 'indent-region-or-buffer))))

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


;;; While we are in the topic of prompting, a lot of the default prompts ask
;;; for a yes or a no. I’m lazy and so I don’t want to type the full words.
;;; Let’s just make it accept y or n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'dired-x)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)

;;; remove windows end-of-line delimiter
(defun remove-dos-eol ()
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
(defun af-eshell-here ()
  "Go to eshell and set current directory to the buffer's directory."
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name)
                                      default-directory))))
    (if (get-buffer "*eshell*")
	(if (string= (buffer-name) "*eshell*")(delete-window (selected-window))
	  (progn
	    (switch-to-buffer "*eshell*"))
	  )
      (progn
	(split-window-vertically)
	(other-window 1)
	(eshell)
	(eshell/pushd ".")
	(cd dir)
	(goto-char (point-max))
	(eshell-kill-input)
	(eshell-send-input)
	))))


(defun samray/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
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
               (when (and (configuration-layer/package-usedp 'projectile)
                          (projectile-project-p))
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
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-usedp 'projectile)
                   (projectile-project-p))
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
(defun samray/eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
	  '(lambda()
	     (local-set-key (kbd "C-l") 'eshell-clear-buffer)))
(message "loading init-misc")
(provide 'init-misc)
;;; init-misc.el ends here
