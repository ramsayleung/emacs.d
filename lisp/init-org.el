;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package org
  :pin gnu
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

	  (setq org-html-htmlize-output-type nil)

	  ;;Its default value is (ascii html icalendar latex)
	  (setq org-export-backends '(latex icalendar))
	  ;; Show org-edit-special in the other-window
	  (setq org-src-window-setup 'other-window)
	  ;; The directory of storing latax image
	  (setq org-latex-preview-ltxpng-directory "/tmp/org-ltximg")
	  ;; use minted to highlight code in latex
	  (setq org-latex-listings 'minted)
	  (setq org-export-latex-listings 'minted)
	  (add-to-list 'org-latex-packages-alist '("" "minted"))
	  (add-hook 'org-src-mode-hook 'ramsay/disable-flycheck-in-org-src-block)
	  ;; from plan9-theme
	  (custom-set-faces
	   '(org-level-1 ((t (:height 1.4 :weight bold
				      :family "Fantasque Sans Mono"
				      :box (:line-width 1 :style released-button))))))

	  (custom-set-faces
	   '(org-level-2 ((t (:height 1.2
				      :box (:line-width 1 :style released-button))))))
	  (setq org-todo-keyword-faces
		'(
		  ("TODO" . (:foreground "peru" :weight bold))
		  ("STARTED" . (:foreground "DarkOrange" :weight bold))
		  ("DONE" . (:foreground "LimeGreen" :weight bold))
		  ("CANCELLED" . (:foreground "LightGray" :weight bold))
		  ))
	  ;;; PLANNED: A planned thing which contains several TODO items
	  ;;; NEXT: A things need to be done in this week, which might contains several TODO items
	  ;;; few TODO items
	  ;;; TODO: The atomic Todo item , containing a few checklist
	  ;;; STARTED: The started/processing TODO item
	  ;;; DONE: The finished TODO item
	  ;;; CANCELLED: The cancelled TODO item
	  (setq org-todo-keywords
		'((sequence "TODO" "STARTED(!)" "|" "DONE" "CANCELLED")))
	  ;; https://orgmode.org/org.html#Tracking-TODO-state-changes
	  (setq org-log-into-drawer 'LOGBOOK)

	  (setq org-priority-faces '(
				     (?A . (:foreground "red" :weight 'bold))
				     (?B . (:foreground "DarkOrange"))
				     (?C . (:foreground "green"))))

	  ;; Make verbatim with highlight text background.
	  (add-to-list 'org-emphasis-alist
		       '("=" (:background "#fef7ca")))
	  ;; Make deletion(obsolote) text foreground with dark gray.
	  (add-to-list 'org-emphasis-alist
		       '("+" (:foreground "dark gray"
					  :strike-through t)))
	  ;; Make code style around with box.
	  (add-to-list 'org-emphasis-alist
		       '("~" (:box (:line-width 1
						:color "grey75"
						:style released-button))))
	  )
  :config (progn
	    (when (not (eq system-type 'windows-nt))
	      (require 'ox-md nil t)
	      (when (version< "9.2" org-version) (require 'org-tempo))
	      (require 'org-indent)
              ;; (add-hook 'org-mode-hook 'org-indent-mode)
              (setq org-indent-mode-turns-on-hiding-stars nil)
	      ;; 让中文也可以不加空格就使用行内格式, 粗体, 下划线等等
	      (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
	      (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
	      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
	      (org-element-update-syntax)
	      ;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
	      (setq org-use-sub-superscripts "{}")

	      ;; https://emacs-china.org/t/org-9-5-2/19491/3
	      (setq org-adapt-indentation t)

	      ;; let org-mode to delete those auxiliary files after export
	      ;;automatically
	      (setq org-latex-logfiles-extensions (quote
						   ("lof" "lot" "tex" "aux" "idx" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
	      (org-babel-do-load-languages
	       'org-babel-load-languages
	       '((clojure . t)
		 (C . t)
                 (dot . t)
		 (lisp . t)
		 (org . t)
		 (js . t)
		 (latex . t)
		 (ruby . t)
		 (shell . t)
		 (python . t)
		 (plantuml . t)
		 (emacs-lisp . t)
		 (awk . t)
		 (sql . t)
		 (scheme . t)
		 (sqlite . t)))

	      (setq org-startup-with-inline-images t)
	      (setq org-agenda-files '("~/btsync/org"))
	      (setq org-agenda-custom-commands
		    '(
		      ("w" "Weekly Review"
		       agenda ""
		       ((org-agenda-start-with-log-mode '(closed))
			(org-agenda-overriding-header "Weekly Review")
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("PROJ" "DONE" "CANCELLED")))))
		      ))
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
		'(define-auto-insert '(org-mode . "Chinese Export and Hugo skeleton")
		   '("Description: "
		     "#+LATEX_CLASS: ramsay-org-article"\n
		     "#+LATEX_CLASS_OPTIONS: [oneside,A4paper,12pt]"\n
		     "#+AUTHOR: Ramsay Leung"\n
		     "#+EMAIL: ramsayleung@gmail.com"\n
		     "#+DATE: "
		     (format-time-string "%Y-%m-%d %a %H:%M")> \n
		     > _ \n
		     ))
		)

	      ;; Encryption
	      (require 'epa-file)
	      (epa-file-enable)
	      (setq epa-file-select-keys nil) 
	      (require 'org-crypt)
	      (org-crypt-use-before-save-magic)
	      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
	      ;; GPG key to use for encryption
	      ;; Either the Key ID or set to nil to use symmetric encryption.
	      (setq org-crypt-key nil)
	      )
	    )
  )

;; automatically open your agenda when start Emacs
;; (add-hook 'after-init-hook (lambda ()
;; 			     (org-agenda nil "c")))

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/btsync/journal")
  (org-journal-file-format "%Y%m%d.org")
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

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :config
  (setq org-hugo-default-section-directory (concat "post/" (format-time-string "%Y")))
  (setq org-hugo-export-with-section-numbers "num:3")
  :after ox)

;;; Syntax Highlight in html file
(use-package htmlize
  :if (and (not (ramsay/windows-p))
	   window-system)
  :ensure t)

(defun org-file-path (filename)
  "Return the absolute address of an org file FILENAME, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(defun ramsay/disable-flycheck-in-org-src-block ()
  "Disable emacs-lisp-checkdoc in org-src-mode."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(message "loading init-org")
(provide 'init-org)

;;; init-org.el ends here
