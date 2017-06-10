;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package magit
  :commands (magit-stage)
  :ensure t
  )

;;; like a time machine,to move to past versions of the current file
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine-toggle
	     git-timemachine-switch-toggle)
  ;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
  :config (progn

	    (defun samray/git-timemachine-show-selected-revision ()
	      "Show last (current) revision of file."
	      (interactive)
	      (let* ((collection (mapcar (lambda (rev)
					   ;; re-shape list for the ivy-read
					   (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
					 (git-timemachine--revisions))))
		(ivy-read "commits:"
			  collection
			  :action (lambda (rev)
				    ;; compatible with ivy 9+ and ivy 8
				    (unless (string-match-p "^[a-z0-9]*$" (car rev))
				      (setq rev (cdr rev)))
				    (git-timemachine-show-revision rev)))))

	    (defun samray/git-timemachine ()
	      "Open git snapshot with the selected version.  Based on ivy-mode."
	      (interactive)
	      (unless (featurep 'git-timemachine)
		(require 'git-timemachine))
	      (git-timemachine--start #'samray/git-timemachine-show-selected-revision))

	    )
  )

;;; Indicate whether a line has been inserted,modified or deleted.
;;; git-gutter is not compatible with nlinum-mode, so I choose nlinum-mode
;; (use-package git-gutter
;;   :ensure t
;;   :diminish git-gutter-mode
;;   :init (global-git-gutter-mode +1)
;;   )


;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
;; (require 'ivy)
;; (require 'git-gutter)

;; (defun samray/reshape-git-gutter (gutter)
;;   "Re-shape gutter for `ivy-read'."
;;   (let* ((linenum-start (aref gutter 3))
;;          (linenum-end (aref gutter 4))
;;          (target-line "")
;;          (target-linenum 1)
;;          (tmp-line "")
;;          (max-line-length 0))
;;     (save-excursion
;;       (while (<= linenum-start linenum-end)
;;         (goto-line linenum-start)
;;         (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
;;                                                  (buffer-substring (line-beginning-position)
;;                                                                    (line-end-position))))
;;         (when (> (length tmp-line) max-line-length)
;;           (setq target-linenum linenum-start)
;;           (setq target-line tmp-line)
;;           (setq max-line-length (length tmp-line)))

;;         (setq linenum-start (1+ linenum-start))))
;;     ;; build (key . linenum-start)
;;     (cons (format "%s %d: %s"
;;                   (if (eq 'deleted (aref gutter 1)) "-" "+")
;;                   target-linenum target-line)
;;           target-linenum)))

;; (defun samray/goto-git-gutter ()
;;   (interactive)
;;   (if git-gutter:diffinfos
;;       (ivy-read "git-gutters:"
;;                 (mapcar 'samray/reshape-git-gutter git-gutter:diffinfos)
;;                 :action (lambda (e)
;;                           ;; ivy9+ keep `(car e)'
;;                           ;; ivy8- strip the `(car e)'
;;                           ;; we handle both data structure
;;                           (unless (numberp e) (setq e (cdr e)))
;;                           (goto-line e)))
;;     (message "NO git-gutters!")))

(provide 'init-version-control)

;;; init-version-control.el ends here
