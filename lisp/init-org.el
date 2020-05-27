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

	  (setq org-ellipsis " [+]")

	  ;;Its default value is (ascii html icalendar latex)
	  (setq org-export-backends '(latex icalendar))
	  ;; Show org-edit-special in the other-window
	  (setq org-src-window-setup 'other-window)
	  ;; use minted to highlight code in latex
	  (setq org-latex-listings 'minted)
	  (setq org-export-latex-listings 'minted)
	  (add-to-list 'org-latex-packages-alist '("" "minted"))
	  (add-hook 'org-src-mode-hook 'ramsay/disable-flycheck-in-org-src-block)
	  (setq org-todo-keyword-faces
		'(
		  ("PROCESSING" . (:foreground "gold"))
		  ))
	  (setq org-todo-keywords
		'((sequence "TODO" "PROCESSING" "DONE")))
	  (setq org-priority-faces '(
				     (?A . (:foreground "red" :weight 'bold))
				     (?B . (:foreground "blue"))
				     (?C . (:foreground "green"))))
	  )
  :config(progn
	   (when (not (eq system-type 'windows-nt))
	     (require 'ox-md nil t)
	     (require 'org-indent)
	     (require 'org-tempo)
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
		(shell . t)
		(python . t)
		(emacs-lisp . t)
		(awk . t)
		(sql . t)
		(sqlite . t)))

	     (setq org-startup-with-inline-images t)
	     (setq org-agenda-files (directory-files "~/btsync/org" t "\\.org"))
	     (setq org-agenda-custom-commands
		   '(("c" "agenda view with alltodo sorted by priorities"
		      ((tags "PRIORITY=\"A\""
			     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			      (org-agenda-overriding-header "High-priority unfinished tasks:")))
		       (agenda "")
		       (alltodo ""
				((org-agenda-skip-function
				  '(or
				    (ramsay/org-skip-subtree-if-priority ?A)
				    (org-agenda-skip-if nil '(scheduled deadline))))))))))
	     (setq org-capture-templates
		   '(("a" "Agenda" entry (file  "~/btsync/org/agenda.org" "Agenda")
		      "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
		     ("w" "Work Note" entry (file+headline "~/btsync/org/work_notes.org" "Notes about work")
		      "** Work Note %?\n%T")
		     ("l" "Not work Note" entry (file+headline "~/btsync/org/notes.org" "Notes not about work")
		      "** %?\n%T")
		     ("j" "Journal" entry (file+datetree "~/btsync/org/journal.org")
		      "** %?\nEntered on %U\n  %i\n  %a")
		     ))


	     ;; Set color for "~"(eg, ~code~)
	     (add-to-list 'org-emphasis-alist
			  '("~" (:foreground "darkseagreen")
			    ))
	     (require 'ox-latex )
	     ;; execute code without confirm
	     (setq org-confirm-babel-evaluate nil)
	     ;; set latex to xelatex
	     (setq org-latex-pdf-process
		   '("xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
		     "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
		     "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"))
	     ;; export cn character
	     (setf org-latex-default-packages-alist
		   (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
	     
	     (eval-after-load 'autoinsert
	       '(define-auto-insert '(org-mode . "Chinese Org skeleton")
		  '("Description: "
		    "#+LATEX_CLASS: ramsay-org-article"\n
		    "#+LATEX_CLASS_OPTIONS: [oneside,A4paper,12pt]"\n
		    "#+AUTHOR: Ramsay Leung"\n
		    "#+EMAIL: ramsayleung@gmail.com"\n
		    "#+DATE: "
		    (format-time-string "%Y-%m-%dT%H:%M:%S")> \n
		    > _ \n
		    )))


	     (defun add-pcomplete-to-capf ()
	       (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
	     (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
	     )
	   )
  )
;; automatically open your agenda when start Emacs
;; (add-hook 'after-init-hook (lambda ()
;; 			     (org-agenda nil "c")))

;;; show org-mode bullets as UTF-8 character
(use-package org-bullets
  :if (and (not (ramsay/windows-p))
	   window-system)
  :defer t
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config (progn
	    (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
	    ))
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/btsync/journal")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-download
  :ensure t
  ;; There is something wrong with `hook`, so redefine it with my own :hook
  :init (add-hook 'org-mode-hook (lambda () (require 'org-download)))
  :config
  (setq-default org-download-image-dir "../images")
  (put 'org-download-image-dir 'safe-local-variable (lambda (_) t)))

;;; Generate table of content.
;;; https://github.com/snosov1/toc-org
(use-package toc-org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'toc-org-mode))

;; Org extra exports
;; Export to github flavored markdown
(use-package ox-gfm
  :if (and (not (eq system-type 'windows-nt))
	   window-system)
  :ensure ox-gfm
  )

(use-package ob-rust
  :if (and (not (ramsay/windows-p))
	   window-system)
  :ensure ob-rust
  )
;;; Export to twitter bootstrap
(use-package ox-twbs
  :if (and (not (ramsay/windows-p))
	   window-system)
  :ensure ox-twbs
  )

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

;;; Syntax Highlight in html file
(use-package htmlize
  :if (and (not (ramsay/windows-p))
	   window-system)
  :ensure t)

;;; Drag and drop images to org-mode
(use-package org-download
  :if (and (not (ramsay/windows-p))
	   window-system)
  :ensure t
  :config (progn
	    (run-with-idle-timer ramsay-idle-time nil 'org-download-enable))
  )

(defun org-file-path (filename)
  "Return the absolute address of an org file FILENAME, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(defun ramsay/disable-flycheck-in-org-src-block ()
  "Disable emacs-lisp-checkdoc in org-src-mode."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(message "loading init-org")
(provide 'init-org)

;;; init-org.el ends here
