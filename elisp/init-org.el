;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package org
  :ensure t
  :config(progn
	   (with-eval-after-load 'org
	     (setq org-agenda-files '("~/Documents/Org/Agenda"))
	     (setq org-capture-templates
		   '(("a" "Agenda" entry (file  "~/Documents/Org/agenga.org" "Agenda")
		      "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
		     ("n" "Note" entry (file+headline "~/Documents/Org/notes.org" "Notes")
		      "* Note %?\n%T")
		     ("l" "Link" entry (file+headline "~/Documents/Org/links.org" "Links")
		      "* %? %^L %^g \n%T" :prepend t)
		     ("b" "Blog idea" entry (file+headline "~/Documents/Org/i.org" "Blog Topics:")
		      "* %?\n%T" :prepend t)
		     ("t" "To Do Item" entry (file+headline "~/Documents/Org/i.org" "To Do Items")
		      "* %?\n%T" :prepend t)
		     ("j" "Journal" entry (file+datetree "~/Documents/Org/journal.org")
		      "* %?\nEntered on %U\n  %i\n  %a")
		     ))
	     ;;GUI Emacs could display image.But if the image is too large,
	     ;;it works so weird
	     (setq org-image-actual-width '(500)) 
	     ;; make sure org-mode syntax highlight source code
	     ;; Make TAB act as if it were issued in a buffer of the
	     ;;language’s major mode.
	     (setq org-src-fontify-natively t
		   org-src-tab-acts-natively t)
	     ;; When editing a code snippet,use the current window rather than
	     ;; popping open a new one
	     (setq org-src-window-setup 'current-window)
	     ;; I like seeing a little downward-pointing arrow instead of the
	     ;;usual ellipsis (...) that org displays when there’s stuff under
	     ;; a header.
	     (setq org-publish-project-alist
		   '(("org-notes"
		      :base-directory "~/Documents/Org/"
		      :publishing-directory "~/Documents/Programming/Html+Css/"
		      :with-sub-superscript nil
		      )))
	     (setq org-ellipsis "⤵")

	     ;;Its default value is (ascii html icalendar latex)
	     (setq org-export-backends '(latex icalendar))
	     ;; Show org-edit-special in the other-window
	     (setq org-src-window-setup 'other-window)
	     (require 'ox-md nil t)
	     (add-hook 'org-src-mode-hook 'samray/disable-flycheck-in-org-src-block)
	     (add-hook 'org-mode-hook #'samray/complete-for-org-mode)
	     )
	   )
  )

;;; pomodoro tech
(use-package org-pomodoro
  :ensure t)

;; ;;; Ultra-minimalist presentation minor-mode for org-mode
;; (use-package org-present
;;   :ensure t
;;   :config (progn
;; 	    (eval-after-load "org-present"
;; 	      '(progn
;; 		 (add-hook 'org-present-mode-hook
;; 			   (lambda ()
;; 			     (org-present-big)
;; 			     (org-display-inline-images)
;; 			     (org-present-hide-cursor)
;; 			     (org-present-read-only)))
;; 		 (add-hook 'org-present-mode-quit-hook
;; 			   (lambda ()
;; 			     (org-present-small)
;; 			     (org-remove-inline-images)
;; 			     (org-present-show-cursor)
;; 			     (org-present-read-write)))))


;; 	    ))

;;; show org-mode bullets as UTF-8 character
(use-package org-bullets
  :ensure t
  :config (progn
	    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
	    (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
	    ))

;;; Org extra exports
;;; Export to github flavored markdown
(use-package ox-gfm
  :ensure t
  :config (progn
	    (with-eval-after-load 'org
	      (require 'ox-gfm nil t)))
  )

;;; Export to twitter bootstrap
(use-package ox-twbs
  :ensure t
  :config (with-eval-after-load 'org
	    (require 'ox-twbs nil t))
  )
;;; Export to reveal for presentation
(use-package ox-reveal
  :ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

;;; Syntax Highlight in html file
(use-package htmlize
  :ensure t)

;;; Drag and drop images to org-mode
(use-package org-download
  :ensure t)


(defun org-file-path (filename)
  "Return the absolute address of an org file FILENAME, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(defun samray/disable-flycheck-in-org-src-block ()
  "Disable emacs-lisp-checkdoc in org-src-mode."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(defun samray/complete-for-org-mode ()
  "Org mode has completion for all keywords via pcomplete,start it."
  (add-hook 'completion-at-point 'pcomplete-completions-at-point nil t))
(provide 'init-org)
;;; init-org.el ends here
