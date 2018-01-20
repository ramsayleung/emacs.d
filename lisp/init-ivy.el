;;; package --- Summary
;;; code:
;;; Commentary:

(use-package counsel
  :ensure t
  :init ()
  )
(use-package swiper
  :ensure t)

(use-package avy
  :commands (avy-goto-char avy-goto-line)
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (progn
	  (setq ivy-re-builders-alist
		'((t . ivy--regex-plus)))
	  (setq ivy-initial-inputs-alist nil)
	  )
  :config
  (ivy-mode 1)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  (defun samray/counsel-goto-recent-directory ()
    "Open recent directory with dired"
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
	   (delete-dups
	    (append (mapcar 'file-name-directory recentf-list)
		    ;; fasd history
		    (if (executable-find "fasd")
			(split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "directories:" collection :action 'dired)))
  )

(use-package counsel-projectile
  :ensure t
  :after projectile
  :init (counsel-projectile-mode)
  )

(use-package ivy-buffer-extend
  :load-path "~/.emacs.d/additional-packages/ivy-buffer-extend.el")

;;; Steal from https://github.com/alexmurray/ivy-xref
(defun ivy-xref-make-collection (xrefs)
  "Transform XREFS into a collection for display via `ivy-read'."
  (let ((collection nil))
    (dolist (xref xrefs)
      (with-slots (summary location) xref
        (let ((line (xref-location-line location))
              (file (xref-location-group location))
              (candidate nil))
          (setq candidate (concat
                           ;; use file name only
                           (car (reverse (split-string file "\\/")))
			   (when (string= "integer" (type-of line))
			     (concat ":" (int-to-string line) ": "))
			   summary))
          (push `(,candidate . ,location) collection))))
    collection))

(defun ivy-xref-show-xrefs (xrefs alist)
  "Show the list of XREFS and ALIST via ivy."
  (let ((buffer (current-buffer)))
    (ivy-read "xref: " (ivy-xref-make-collection xrefs)
              :require-match t
              :sort t
              :action #'(lambda (candidate)
                          (xref--show-location (cdr candidate) 'quit)))
    ;; honor the contact of xref--show-xref-buffer by returning its original
    ;; return value
    buffer))

(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

;;; Sometimes I find too many buffers is distracted
(defun samray/switch-to-current-open-buffer ()
  "Switch to current open bufffer instead of also including;
bookmarks reccently opened files and window layout."
  (interactive)
  (setq ivy-use-virtual-buffers nil)
  (ivy-switch-buffer)
  )

(defun samray/ivy-switch-to-buffer-enhanced ()
  "Ivy-switch-to-buffer with recentf."
  (interactive)
  (setq ivy-use-virtual-buffers t)
  (ivy-switch-buffer))

;; http://emacs.stackexchange.com/questions/10393/how-can-i-answer-a-minibuffer-prompt-from-elisp
(defun samray/insert-symbol-at-point ()
  (if (> (length cached-symbol-at-point) 0)
      (insert cached-symbol-at-point))
  (remove-hook 'post-command-hook 'insert-symbol-at-point)
  )

(defun samray/counsel-ag-symbol-at-point ()
  "Search for number at point using helm-projectile-ag."
  (interactive)
  (setq cached-symbol-at-point (thing-at-point `symbol))
  (add-hook 'post-command-hook 'insert-symbol-at-point)
  (counsel-ag)
  )

(provide 'init-ivy)

;;; init-ivy.el ends here

