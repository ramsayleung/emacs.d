;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package magit
  :commands (magit-stage magit-status)
  :ensure t
  :init (progn
	  ;;; Force redisplay git branch of the mode line.
	  (setq auto-revert-check-vc-info t)

	  ;; Disable version control checking for Tramp.
	  (setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))
	  )
  )

;;; like a time machine,to move to past versions of the current file
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine-toggle
	     git-timemachine-switch-toggle)
  ;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
  :config (progn
	    (defun ramsay/git-timemachine-show-selected-revision ()
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

	    (defun ramsay/git-timemachine ()
	      "Open git snapshot with the selected version.  Based on ivy-mode."
	      (interactive)
	      (unless (featurep 'git-timemachine)
		(require 'git-timemachine))
	      (git-timemachine--start #'ramsay/git-timemachine-show-selected-revision))

	    )
  )

(message "loading init-version-control")
(provide 'init-version-control)

;;; init-version-control.el ends here
