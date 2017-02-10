;;; package --- Summary
;;; code:
;;; Commentary:

;; make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t)

;; help you use shell easily on Emacs
(use-package shell-pop
  :ensure t)

(use-package youdao-dictionary
  :ensure t
  :config (progn
	    (setq url-automatic-caching t)
	    (evil-leader/set-key
	      "o y" 'youdao-dictionary-search-at-point)
	    ))

;;; read pdf file in Emacs
(use-package pdf-tools
  :ensure t)

;;; use Irc in Emacs
(use-package circe
  :ensure t
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
;;; treat undo history as a tree
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t))
;; enhance dired
;; (require 'dired+)
;; (diredp-toggle-find-file-reuse-dir t)

;; dired reuse directory buffer https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))
					; was dired-up-directory
	    ))
(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
	      ;; (filename (dired-get-filename))
	      (filename (dired-get-filename t t))
	      (bye-p (file-directory-p filename)))
	 ad-do-it
	 (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
	   (kill-buffer orig))))))

;; Emacs extension to increate selected region by semantic units
(use-package expand-region
  :ensure t
  :config())

;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
(use-package popwin
  :ensure t
  :config(popwin-mode t))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(delete-selection-mode t)
(setq make-backup-files nil)
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

(abbrev-mode t)
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
			    (unless (derived-mode-p '(python-mode markdown-mode))
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


(fset 'yes-or-no-p 'y-or-n-p)

(require 'dired-x)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)

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
(set-language-environment "UTF-8")
(setq x-select-enable-clipboard-manager nil)
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
(use-package restart-emacs
  :ensure t
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
  (evil-leader/set-key
    "qd" 'samray/restart-emacs-debug-init
    "qD" 'samray/restart-stock-emacs-with-packages
    "qr" 'samray/restart-emacs-resume-layouts
    "qR" 'samray/restart-emacs))

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

;;; record how much time i use to programming with wakatimw
(use-package wakatime-mode
  :ensure t
  :config (global-wakatime-mode))

;;; Chinese input method ,clone from https://github.com/tumashu/chinese-pyim
(use-package chinese-pyim
  :ensure t
  :config
  ;; 激活 basedict 拼音词库
  (use-package chinese-pyim-basedict
    :ensure t
    :config (chinese-pyim-basedict-enable))


  (setq default-input-method "chinese-pyim")

  ;; 使用全拼
  (setq pyim-default-scheme 'quanpin)
  ;; 开启拼音搜索功能
  (setq pyim-isearch-enable-pinyin-search t)

  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 8)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  )
(provide 'init-misc)
;;; init-misc.el ends here
