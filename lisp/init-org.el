;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init (progn
	  ;;each time you turn an entry from a TODO (not-done) state
	  ;;into any of the DONE states, a line ‘CLOSED: [timestamp]’ will
	  ;;be inserted just after the headline
	  (setq org-log-done 'time)
	  ;;GUI Emacs could display image.But if the image is too large,
	  ;;it works so weird
	  (setq org-image-actual-width '(500))
	  ;; make sure org-mode syntax highlight source code
	  ;; Make TAB act as if it were issued in a buffer of the
	  ;;language’s major mode.
	  (setq org-src-fontify-natively t
		org-src-tab-acts-natively t)

	  ;; I like seeing a little downward-pointing arrow instead of the
	  ;;usual ellipsis (...) that org displays when there’s stuff under
	  ;; a header.
	  (setq org-ellipsis "⤵")

	  ;;Its default value is (ascii html icalendar latex)
	  (setq org-export-backends '(latex icalendar))
	  ;; Show org-edit-special in the other-window
	  (setq org-src-window-setup 'other-window)
	  ;; use minted to highlight code in latex
	  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
	  ;; (setq org-latex-listings 'minted)
	  (add-hook 'org-src-mode-hook 'samray/disable-flycheck-in-org-src-block)
	  (setq org-todo-keyword-faces
		'(
		  ("NOTDO" . (:foreground "black"))
		  ("PROCESSING" . (:foreground "gold" :weight bold))
		  ))
	  (setq org-todo-keywords
		'((sequence "NOTDO" "TODO" "PROCESSING" "DONE")))
	  (setq org-priority-faces '(
				     (?A . (:foreground "red" :weight 'bold))
				     (?B . (:foreground "blue"))
				     (?C . (:foreground "green"))))
	  (defun samray/org-skip-subtree-if-priority (priority)
	    "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
	    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
		  (pri-value (* 1000 (- org-lowest-priority priority)))
		  (pri-current (org-get-priority (thing-at-point 'line t))))
	      (if (= pri-value pri-current)
		  subtree-end
		nil)))
	  )
  :config(progn
	   (when (not (samray/is-windows))
	     (require 'ox-md nil t)
	     (require 'org-indent)
	     (org-indent-mode -1)         ;disable org-indent-mode
	     ;; let org-mode to delete those auxiliary files after export
	     ;;automatically
	     (setq org-latex-logfiles-extensions (quote
						  ("lof" "lot" "tex" "aux" "idx" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
	     (org-babel-do-load-languages
	      'org-babel-load-languages
	      '((clojure . t)
		(lisp . t)
		(org . t)
		(js . t)
		(latex . t)
		(ruby . t)
		(sh . t)
		(python . t)
		(emacs-lisp . t)
		(awk . t)
		(sql . t)
		(sqlite . t)))

	     (setq org-agenda-files '("~/Dropbox/Org/agenda.org" "~/Dropbox/Org/todo.org"))
	     (setq org-agenda-custom-commands
		   '(("c" "agenda view with alltodo sorted by priorities"
		      ((tags "PRIORITY=\"A\""
			     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			      (org-agenda-overriding-header "High-priority unfinished tasks:")))
		       (agenda "")
		       (alltodo ""
				((org-agenda-skip-function
				  '(or
				    (samray/org-skip-subtree-if-priority ?A)
				    (org-agenda-skip-entry-if 'todo '("NOTDO"))
				    (org-agenda-skip-if nil '(scheduled deadline))))))))))
	     (setq org-capture-templates
		   '(("a" "Agenda" entry (file  "~/Dropbox/Org/agenda.org" "Agenda")
		      "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
		     ("n" "Note" entry (file+headline "~/Dropbox/Org/notes.org" "Notes")
		      "* Note %?\n%T")
		     ("l" "Link" entry (file+headline "~/Dropbox/Org/links.org" "Links")
		      "* %? %^L %^g \n%T" :prepend t)
		     ("b" "Blog idea" entry (file+headline "~/Dropbox/Org/blog.org" "Blog Topics:")
		      "* %?\n%T" :prepend t)
		     ("t" "To Do Item" entry (file+headline "~/Dropbox/Org/todo.org" "To Do Items")
		      "* TODO  %?\n  %i\n" :prepend t)
		     ("j" "Journal" entry (file+datetree "~/Dropbox/Org/journal.org")
		      "* %?\nEntered on %U\n  %i\n  %a")
		     ))


	     ;; Set color for "~"(eg, ~code~)
	     (add-to-list 'org-emphasis-alist
			  '("~" (:foreground "darkseagreen")
			    ))
	     (require 'ox-latex )
	     (setq org-export-latex-listings t)
	     ;;org-mode source code setup in exporting to latex
	     (add-to-list 'org-latex-listings '("" "listings"))
	     (add-to-list 'org-latex-listings '("" "color"))

	     ;; execute code without confirm
	     (setq org-confirm-babel-evaluate nil)
	     ;; set latex to xelatex
	     (setq org-latex-pdf-process
		   '("xelatex  --shell-escape -interaction nonstopmode -output-directory %o %f"
		     ;; "biber %b" "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
		     "bibtex %b"
		     "xelatex  -interaction nonstopmode -output-directory %o %f"
		     "xelatex  -interaction nonstopmode -output-directory %o %f"))
	     ;; export cn character
	     (setf org-latex-default-packages-alist
		   (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
	     

	     (defun add-pcomplete-to-capf ()
	       (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
	     (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
	     )
	   )
  )
;; automatically open your agenda when start Emacs
(add-hook 'after-init-hook (lambda ()
			     (org-agenda nil "c")))

;;; pomodoro tech
(use-package org-pomodoro
  :if (and (not (samray/is-windows))
	   window-system)
  :commands (org-pomodoro)
  :ensure t)

;;; for journal
(use-package org-journal
  :if (and (not (samray/is-windows))
	   window-system)
  :ensure t
  :commands (org-journal-new-entry)
  :after org
  :init (progn
	  (setq org-journal-dir "~/Dropbox/journal")
	  )
  )

;;; show org-mode bullets as UTF-8 character
(use-package org-bullets
  :if (and (not (samray/is-windows))
	   window-system)
  :after org
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config (progn
	    (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
	    ))

;; Org extra exports
;; Export to github flavored markdown
(use-package ox-gfm
  :if (and (not (samray/is-windows))
	   window-system)
  :ensure ox-gfm
  )

;;; Export to twitter bootstrap
(use-package ox-twbs
  :if (and (not (samray/is-windows))
	   window-system)
  :ensure ox-twbs
  )

;;; Export to reveal for presentation
;; (use-package ox-reveal
;;   :if (and (not (samray/is-windows))
;; 	   window-system)
;;   :ensure ox-reveal)

;; ;;; fix org-mode/latex chinese char issue
;; (use-package ox-latex-chinese
;;   :ensure t
;;   :init (progn
;; 	  (require 'ox-latex-chinese)
;; 	  (oxlc/toggle-ox-latex-chinese t)
;; 	  ))
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

;;; Syntax Highlight in html file
(use-package htmlize
  :if (and (not (samray/is-windows))
	   window-system)
  :ensure t)

;;; Drag and drop images to org-mode
(use-package org-download
  :if (and (not (samray/is-windows))
	   window-system)
  :ensure t)

(defun org-file-path (filename)
  "Return the absolute address of an org file FILENAME, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(defun samray/disable-flycheck-in-org-src-block ()
  "Disable emacs-lisp-checkdoc in org-src-mode."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(provide 'init-org)

;;; init-org.el ends here
