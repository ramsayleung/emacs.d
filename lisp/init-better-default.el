;;; package --- Summary
;;; code:
;;; Commentary:

;;;enhance dired
;; (use-package dired+
;;   :ensure t
;;   :init
;;   (diredp-toggle-find-file-reuse-dir t)
;;   )
(setq dired-open-extensions
      '(("mkv" . "vlc")
        ("mp4" . "vlc")
        ("avi" . "vlc")))
;;; From http://oremacs.com/2015/01/12/dired-file-size/
(defun dired-get-size ()
  "Get size of specified file."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))
;;;to get rid of the garbage produced by a LaTeX 
(setq dired-garbage-files-regexp
      "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")

;;; Nerd tree for Emacs
(use-package neotree
  :ensure t
  :commands (neotree-toggle)
  :init (progn
	  ;; set neotree theme
	  (setq neo-theme 'nerd)
	  ;; Every time when the neotree window is opened, let it find current file and jump to node.
	  (setq neo-smart-open t)
	  ;; When running ‘projectile-switch-project’, ‘neotree’ will change root automatically.
	  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
	  ;;  NeoTree can be opened (toggled) at projectile project root 
	  (defun neotree-project-dir ()
	    "Open NeoTree using the git root."
	    (interactive)
	    (let ((project-dir (projectile-project-root))
		  (file-name (buffer-file-name)))
	      (neotree-toggle)
	      (if project-dir
		  (if (neo-global--window-exists-p)
		      (progn
			(neotree-dir project-dir)
			(neotree-find file-name)))
		(message "Could not find git project root."))))
	  ;; If you use evil-mode, by default some of evil key bindings conflict with neotree-mode keys.
	  (eval-after-load 'evil
	    '(progn
	       (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
	       (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
	       (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
	       (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
	       ))
	  ))

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


(require 'dired-x)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(defvar dired-compress-files-alist
  '(("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o")
    ("\\.zip\\'" . "zip %o -r --filesync %i"))
  "Control the compression shell command for `dired-do-compress-to'.

Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD the the
corresponding command.

Within CMD, %i denotes the input file(s), and %o denotes the
output file. %i path(s) are relative, while %o is absolute.")

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
