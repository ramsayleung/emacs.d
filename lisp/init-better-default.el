;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; code:
;;; Commentary:

;;; Auto save file when Emacs idle
(auto-save-visited-mode +1)
(setq auto-save-visited-interval 1)

;;; Never delete *scratch* buffer
;;; https://emacs.stackexchange.com/questions/19254/never-close-scratch
(add-hook 'kill-buffer-query-functions
          (lambda() (not (equal (buffer-name) "*scratch*"))))

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


(use-package ediff
  :ensure t
  :init
  ;; ignore whitespace
  (setq ediff-diff-options "-w")
  ;; split side by side
  (setq ediff-split-window-function 'split-window-horizontally)
  ;; prevent popup
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; restore the windows after ediff quit
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  )


(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;;; Steal from https://emacstalk.github.io/post/010/
;;; Use `fd` command to find file.
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

;;; pixel scroll
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-interpolate-page t)
  (defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command 'pixel-scroll-interpolate-up))

;;; native-comp
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    ))

;;; While we are in the topic of prompting, a lot of the default prompts ask
;;; for a yes or a no. I’m lazy and so I don’t want to type the full words.
;;; Let’s just make it accept y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;; xref
(when (boundp 'xref-search-program)
  (setq xref-search-program 'ripgrep))

;;; set "Meta" key to be the mac command key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)


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

(use-package ibuffer
  :config
  (setq ibuffer-expert t) ; stop yes no prompt on delete
)
(message "loading init-better-default")
(provide 'init-better-default)
;;; init-better-default.el ends here
