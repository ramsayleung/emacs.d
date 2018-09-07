;;; package --- Summary: -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :demand t
  :commands (company-mode
             company-complete
             company-complete-common
             company-complete-common-or-cycle
             company-files
             company-dabbrev
             company-jedi
             company-tern
             company-web-html
             )
  :init (setq
	 company-global-modes '(not org-mode)
	 company-minimum-prefix-length 2
	 company-require-match 0
	 company-selection-wrap-around t
	 company-dabbrev-downcase nil
	 company-echo-delay 0                          ; remove annoying blinking
	 company-tooltip-limit 20                      ; bigger popup window
	 company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
	 company-idle-delay .4                         ; decrease delay before autocompletion popup shows
	 company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  :config (progn
	    (global-company-mode t))
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'company-mode-hook 'company-quickhelp-mode)
	  ))

(use-package company-statistics
  :ensure t
  :defer t
  :init (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-anaconda
  :ensure t
  :defer t
  :init(progn
	 (add-hook 'python-mode-hook
		   (lambda()(add-to-list 'company-backends 'company-anaconda)))
	 ))

;; company-mode completion back-end for python JEDI
(use-package company-jedi
  :ensure t
  :defer t
  :init(progn
	 (add-hook 'python-mode-hook
		   (lambda()(add-to-list 'company-backends 'company-jedi)))
	 ))

;; HTML completion
(use-package company-web
  :ensure t
  :defer t
  :init (add-hook 'web-mode-hook
		  (lambda () (add-to-list 'company-backends 'company-web-html))))

;;; Shell Script completion
(use-package company-shell
  :ensure t
  :defer t
  :init (add-hook 'sh-mode-hook
		  (lambda () (add-to-list 'company-backends 'company-shell))))

;;; C/C++ headers completion
(use-package company-c-headers
  :ensure t
  :defer t
  :init (progn
	  (cond ((eq system-type 'gnu/linux)
		 (setq company-c-headers-path-system '("/usr/include/c++/7" "/usr/include" "/usr/local/include")))
		((eq system-type 'darwin)
		 (setq company-c-headers-path-system '("/usr/include/c++/4.2.1" "/usr/include" "/usr/local/include")))
		((eq system-type 'windows-nt)
		 ))

	  (add-hook 'c-mode-hook
		    (lambda () (add-to-list 'company-backends 'company-c-headers))
		    )
	  (add-hook 'c++-mode-hook
		    (lambda ()
		      (add-to-list 'company-backends 'company-c-headers)
		      ))
	  )
  )
;;; backends for irony
(use-package company-irony
  :ensure t
  :defer t
  :init (progn (add-hook 'c-mode-hook
			 (lambda () (add-to-list 'company-backends 'company-irony)))
	       (add-hook 'c++-mode-hook
			 (lambda () (add-to-list 'company-backends 'company-irony)))
	       ))

;;; backends for irony-c-header
(use-package company-irony-c-headers
  :ensure t
  :defer t
  :init (progn (add-hook 'c-mode-hook
			 (lambda () (add-to-list 'company-backends 'company-irony-c-headers)))
	       (add-hook 'c++-mode-hook
			 (lambda () (add-to-list 'company-backends 'company-irony-c-headers)))
	       ))

;;; backends for restclient
(use-package company-restclient
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'restclient-mode-hook
		    (lambda () (add-to-list 'company-backends 'company-restclient)))))
;;; backends for go
(use-package company-go
  :load-path "~/.emacs.d/additional-packages/company-go.el"
  :ensure t
  :init (progn
	  (add-hook 'go-mode-hook
	  	    (lambda () (add-to-list 'company-backends 'company-go)))
	  )
  )
;; backends for rust
(use-package company-racer
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'rust-mode-hook (lambda ()
				      (add-to-list 'company-backends 'company-racer)
				      ))
	  ))

;;; backend for rtags
(use-package company-rtags
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'c-mode-hook (lambda ()
				   (add-to-list 'company-backends 'company-rtags)))

	  (add-hook 'c++-mode-hook (lambda ()
				     (add-to-list 'company-backends 'company-rtags)))
	  ))
;;; backends for lsp-mode
(use-package company-lsp
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'lsp-mode-hook (lambda ()
				     (add-to-list 'company-backends 'company-lsp)))
	  ))

(provide 'init-auto-completion)
;;; init-auto-completion.el ends here
