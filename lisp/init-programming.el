;;; package --- Summary:
;;; Commentary:
;;; Code:
;;; Commentary:

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "γ")
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet)
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (progn
	    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
	    ))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package json-mode
  :ensure t
  :mode "\\.json$")

(use-package nginx-mode
  :ensure t
  :commands (nginx-mode))

;; make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t
  :init (progn
	  (setq exec-path-from-shell-variables '("RUST_SRC_PATH" "PYTHONPATH"))
	  ;; when it is nil, exec-path-from-shell will read environment variable
	  ;; from .zshenv instead of .zshrc, but makes sure that you put all
	  ;; environment variable you need in .zshenv rather than .zshrc
	  (setq exec-path-from-shell-check-startup-files nil) ;
	  (setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
	  (exec-path-from-shell-initialize)
	  )
  )

;; Emacs extension to increate selected region by semantic units
(use-package expand-region
  :ensure t
  :commands er/expand-region
  )
;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

;;; jump to definition package
(use-package dumb-jump
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'prog-mode-hook 'dumb-jump-mode)
	  )
  :config (setq dumb-jump-selector 'ivy))

;;; Rest client in Emacs
(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode))

;;; Evil is not especilly useful in the terminal,so
(evil-set-initial-state 'term-mode 'emacs)

(use-package projectile-speedbar
  :load-path "~/.emacs.d/additional-packages/projectile-speedbar.el"
  :ensure t
  :commands (projectile-speedbar-open-current-buffer-in-tree
	     projectile-speedbar-toggle)
  )

(use-package sr-speedbar
  :load-path "~/.emacs.d/additional-packages/sr-speedbar.el"
  :commands (sr-speedbar-toggle)
  :init (progn
	  (setq speedbar-use-images nil)
	  (setq sr-speedbar-right-side nil)
	  (setq sr-speedbar-auto-refresh nil)
	  (setq speedbar-show-unknown-files t)
	  (setq speedbar-directory-unshown-regexp "^\\(\\.\\.?\\)$")
	  )
  :config (progn
	    (add-hook 'speedbar-mode-hook (lambda () (linum-mode -1)))
	    ))
(use-package symbol-overlay
  :ensure t
  :commands (symbol-overlay-put)
  )

(defun samray/speedbar-contract-all-lines ()
  "Contract all items in the speedbar buffer."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line)
    (speedbar-contract-line)))

(defun samray/projectile-speedbar-toggle ()
  "Improve the default projectile speedbar toggle."
  (interactive)
  (if (buffer-file-name)
      (let ((current-buffer (buffer-name)))
	(sr-speedbar-toggle)
	(if (sr-speedbar-exist-p)
	    (progn
	      (set-buffer current-buffer)
	      (projectile-speedbar-open-current-buffer-in-tree)
	      )
	  ))
    (progn
      (sr-speedbar-toggle)
      (sr-speedbar-refresh)
      )))

(defun samray/speedbar-toggle ()
  "Expand current file in speedbar buffer."
  (interactive)
  (if (buffer-file-name)
      (let ((current-buffer (buffer-name)))
	(cond ((sr-speedbar-exist-p) (kill-buffer speedbar-buffer))
	      (t (sr-speedbar-open) (linum-mode -1) (speedbar-refresh)))
	(set-buffer current-buffer)
	(imenu-list-smart-toggle)
	)
    (progn
      (cond ((sr-speedbar-exist-p) (kill-buffer speedbar-buffer))
	    (t (sr-speedbar-open) (linum-mode -1) (speedbar-refresh)))
      )))
(add-hook 'imenu-list-major-mode-hook (lambda () (linum-mode -1))
	  ;; 
	  )

(defun samray/term-paste (&optional string)
  "Yanking in the term-mode doesn't quit work The text from the
   paste appears in the buffer but isn't sent to the shell
"
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string
     (current-kill 0)))
  )
(add-hook 'term-mode-hook
	  (lambda ()
	    (goto-address-mode)
	    (setq yas-dont-activate-functions t)))

;;; https://www.emacswiki.org/emacs/AutoFillMode
;;; auto format comment to 80-char long
(setq-default fill-column 80)

(defun comment-auto-fill ()
  "Auto fill comments but not code in programmingModes."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'comment-auto-fill))

(defun samray/switch-to-buffer (repl-buffer-name)
  "Run REPL and switch to the  buffer REPL-BUFFER-NAME.
similar to shell-pop"
  (interactive)
  (if (get-buffer repl-buffer-name)
      (if (string= (buffer-name) repl-buffer-name)
	  (if (not (one-window-p))
	      (progn (bury-buffer)
		     (delete-window))
	    )
	(progn (samray/split-window-below-and-move)
	       (switch-to-buffer repl-buffer-name)
	       (goto-char (point-max))
	       (evil-insert-state)))
    (progn
      (run-python)
      (samray/split-window-below-and-move)
      (switch-to-buffer repl-buffer-name)
      (goto-char (point-max))
      (evil-insert-state))))

(defun samray/repl-pop ()
  "Run REPL for different major mode and switch to the repl buffer.
similar to shell-pop"
  (interactive)
  (let* ((repl-modes '((python-mode . "*Python*")
		       (scheme-mode . "* Guile REPL *"))))
    (cond ((or (derived-mode-p 'python-mode) (derived-mode-p 'inferior-python-mode))
	   (progn
;;; To fix issue that there is weird eshell output with ipython
	     (samray/switch-to-buffer (cdr (assoc 'python-mode repl-modes)))))
	  ((or (derived-mode-p 'scheme-mode) (derived-mode-p 'geiser-repl-mode))
	   (samray/switch-to-buffer (cdr (assoc 'scheme-mode repl-modes))))
	  ((or (derived-mode-p 'prog-mode)(derived-mode-p 'inferior-python-mode))
	   (progn
	     (samray/switch-to-buffer (cdr (assoc 'python-mode repl-modes)))))
	  )))
;;; Treating terms in CamelCase symbols as separate words makes editing a
;;; little easier for me, so I like to use subword-mode everywhere.
;;;  Nomenclature           Subwords
;; ===========================================================
;; GtkWindow          =>  "Gtk" and "Window"
;; EmacsFrameClass    =>  "Emacs", "Frame" and "Class"
;; NSGraphicsContext  =>  "NS", "Graphics" and "Context"
(global-subword-mode t)
;;; Put *compilation* buffer in the bottom of window which will disappears
;;; automatically,instead shows in other window
(setq compilation-scroll-output t)

;;; From http://blog.binchen.org/posts/turn-off-linum-mode-when-file-is-too-big.html
(defun samray/buffer-too-big-p ()
  "Turn off 'linum-mode' when file is too big."
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))
(add-hook 'prog-mode-hook
          (lambda ()
            ;; turn off `linum-mode' when there are more than 5000 lines
            (if (samray/buffer-too-big-p) (linum-mode -1))))
(provide 'init-programming)

;;; init-programming.el ends here
