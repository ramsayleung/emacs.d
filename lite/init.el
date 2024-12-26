;;; init.el --- g                                    -*- lexical-binding: t; -*-
;;; Run the following command to use this configuration
;;; emacs ---init-directory ~/.emacs.d/lite
;; Copyright (C) 2024  Ramsay Leung
;; Author: Ramsay Leung <ramsayleung@gmail.com>

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t
              use-package-always-ensure t
              indent-tabs-mode nil
              tab-width 2
              css-indent-offset 2)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist
	           '(font . "-PfEd-Fantasque Sans Mono-regular-normal-normal-*-19-*-*-*-m-0-iso10646-1"))

(setq make-backup-files nil
      auto-save-default nil
      inhibit-splash-screen t
      visible-bell nil)

(menu-bar-mode -1)
(tool-bar-mode -1)


(setq ring-bell-function #'ignore)

(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init (progn
	        (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
	        (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
	        (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
	        (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
	        (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
	        (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
	        (add-hook 'racket-mode-hook           #'enable-paredit-mode)
          ;;; Auto complete pair symbol, such as `()`, `{}`
	        (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook lisp-interaction-mode-hook python-mode-hook rust-mode-hook c++-mode-hook racket-mode-hook))
 	          (add-hook hook 'electric-pair-mode))
	        ))


(use-package flycheck
  :ensure t
  :demand t
  :config
  (setq flycheck-mode-line-prefix "FC")
  (global-flycheck-mode t))

(use-package magit
  :commands (magit-stage magit-status)
  :ensure t
  :init (progn
	  ;;; Force redisplay git branch of the mode line.
	        (setq auto-revert-check-vc-info t)
	        )
  )

;;; emacs > 28
(use-package project
  :ensure t
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))
  (defun ramsay/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'ramsay/project-files-in-directory
            (or dirs (list (project-root project))))))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config (progn
            (add-hook 'markdown-mode-hook 'visual-line-mode)))

(use-package which-key
  :defer 1
  :config (which-key-mode))

(setq dired-dwim-target t
      dired-recursive-deletes t
      dired-use-ls-dired nil
      delete-by-moving-to-trash t)

;;; Handle long line
;;; https://emacs-china.org/t/topic/25811/7
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup nil))

(use-package org
  :pin gnu
  :ensure t
  :mode ("\\.org\\'" . org-mode))

(use-package evil
  :ensure t
  :demand t
  :init (progn
	        (setq evil-respect-visual-line-mode t)
	        (setq evil-want-C-u-scroll t))
  :config
  (evil-mode t)
    ;;; modify evil-state-tag
  (setq evil-normal-state-tag   (propertize "[Normal]")
	      evil-emacs-state-tag    (propertize "[Emacs]")
	      evil-insert-state-tag   (propertize "[Insert]")
	      evil-motion-state-tag   (propertize "[Motion]")
	      evil-visual-state-tag   (propertize "[Visual]")
	      evil-operator-state-tag (propertize "[Operator]"))
  (setq evil-insert-state-cursor '(box))
  (setq evil-undo-system 'undo-tree))

;;;Move evil tag to beginning of mode line
(setq evil-mode-line-format '(before . mode-line-front-space))
(setq-default mode-line-buffer-identification
              (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat (when (buffer-file-name) (concat (file-name-nondirectory (directory-file-name default-directory)) "/" ))  ,orig )
                              (cdr mode-line-buffer-identification)))))

;;; set "Meta" key to be the mac command key
(setq mac-option-key-is-meta nil
	    mac-command-key-is-meta t
	    mac-command-modifier 'meta
	    mac-option-modifier 'none)

(use-package counsel
  :ensure t
  )

(use-package company
  :ensure t
  :diminish company-mode

  :init (setq
	       company-minimum-prefix-length 2
	       company-require-match nil
	       company-selection-wrap-around t
	       company-echo-delay 0                          ; remove annoying blinking
	       company-tooltip-limit 10                      ; bigger popup window
	       company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
	       company-idle-delay .15                         ; decrease delay before autocompletion popup shows
	       company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (global-company-mode t)
  )

(use-package swiper
  :ensure t
  :bind (("\C-s" . swiper))
  )

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :diminish ivy-mode
  :config
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  ;; Use grep instead of grep
  (setq counsel-grep-base-command
	      "rg -i -M 120 --no-heading --line-number --color never %s %s")
  ;; Count candidates
  (setq ivy-count-format "%d/%d ")
  ;; Number of result lines to display, set height as width-body-height/2
  (setq ivy-height 25))

(use-package acme-theme
  :ensure t
  :init
  :defer t)

;;; Disable theme before load a new theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  "Disable theme before load theme."
  (mapc #'disable-theme custom-enabled-themes))

;; (load-theme 'modus-operandi t)
(load-theme 'acme t)

;;; Use default line-number-mode instead of nlinum or linum (require Emacs >= 26).
;; (setq-default display-line-numbers-width 1)
(setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode t)

;; number of characters until the fill column
(setq fill-column 120)
;; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  :config (global-undo-tree-mode t)
  :bind (("C-x u" . undo-tree-visualize))
  )

(require 'server)
(unless (server-running-p) (server-start))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(diminish evil fido-vertical-mode which-key markdown-mode magit flycheck)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
