;;; package --- Summary
;;; code:
;;; Commentary:

;;; auto save file when Emacs idle
(use-package auto-save
  :load-path "~/.emacs.d/additional-packages/auto-save.el"
  :config (progn
            (auto-save-enable)
            (setq auto-save-slient t)
            (setq auto-save-idle 1)
            ))

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

;;; File encoding system
;;; UTF-8 works for most of the files i tend to used
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)
(set-language-environment "UTF-8")
(setq x-select-enable-clipboard-manager nil)

;;; control how emacs makes backup files
;; (setq make-backup-files nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(delete-selection-mode t)

;;; While we are in the topic of prompting, a lot of the default prompts ask
;;; for a yes or a no. I’m lazy and so I don’t want to type the full words.
;;; Let’s just make it accept y or n
(fset 'yes-or-no-p 'y-or-n-p)



;;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-use-ssh-controlmaster-options nil)

;;; set "Meta" key to be the mac command key
(when (memq window-system '(mac ns))
  (setq mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none)
  )
(provide 'init-better-default)
;;; init-better-default.el ends here
