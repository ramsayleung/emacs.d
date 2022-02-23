;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; code:
;;; Commentary:

;;; auto save file when Emacs idle
(use-package auto-save
  :load-path "~/.emacs.d/additional-packages/auto-save.el"
  :config (progn
            (auto-save-enable)
            (setq auto-save-slient t)
            (setq auto-save-idle 3)
            ))
(use-package valign
  :load-path "~/.emacs.d/additional-packages/valign.el"
  :diminish valign-mode
  :hook (org-mode . valign-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup nil))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package dired-sidebar
  :bind (([f8] . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

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

;;; native-comp
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    ))

;;; Some information about myself.
(setq user-full-name "Ramsay Leung")
(setq user-mail-address "ramsayleung@gmail.com")

;;; While we are in the topic of prompting, a lot of the default prompts ask
;;; for a yes or a no. I’m lazy and so I don’t want to type the full words.
;;; Let’s just make it accept y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-use-ssh-controlmaster-options nil)

;;; xref
(when emacs/>=28p  (setq xref-search-program 'ripgrep))

;;; set "Meta" key to be the mac command key
(when (ramsay/mac-os-p)
  (setq mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))


(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
	(if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
	    (progn
	      (make-directory dir)
	      )
	  (keyboard-quit))
	))))

;;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(message "loading init-better-default")
(provide 'init-better-default)
;;; init-better-default.el ends here
