;;; package --- Summary
;;; code:
;;; Commentary:

;;;----------------;;;
;;;    Web Mode    ;;;
;;;----------------;;;
;;; web-beautify is a formatting package of Html Css and Javascript/Json
;; for Emacs

(use-package web-beautify
  :ensure t
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer)
  :init(progn
	 (add-hook 'js2-mode-hook
		   (lambda () (unless (derived-mode-p 'vue-mode)
				(add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
	 ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
	 (add-hook 'js-mode-hook
		   (lambda () (unless (derived-mode-p 'vue-mode)
				(add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

	 (add-hook 'json-mode-hook
		   (lambda () (unless (derived-mode-p 'vue-mode)
				(add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

	 (add-hook 'html-mode-hook
		   (lambda () (unless (derived-mode-p 'vue-mode)
				(add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

	 (add-hook 'web-mode-hook
		   (lambda () (unless (derived-mode-p 'vue-mode)
				(add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

	 (add-hook 'css-mode-hook
		   (lambda () (unless (derived-mode-p 'vue-mode)
				(add-hook 'before-save-hook 'web-beautify-css-buffer t t))))
	 ))
;;  an autonomous emacs major-mode for editing web template
;; Html documents can embed parts (css/javascript) and blocks(client/server side)
(use-package web-mode
  :ensure t
  :mode (
	 ("\\.html\\'" . web-mode)
	 )
  :init(progn
	 (add-hook 'web-mode-hook 'samray/web-mode-indent-setup)
	 ))

;; config for web-mode
(defun samray/web-mode-indent-setup()
  (setq web-mode-markup-indent-offset 2) ;web-mode,html tag in html file
  (setq web-mode-css-indent-offset 2)	 ;web-mode,css in html file
  (setq web-mode-code-indent-offset 2)	 ;web-mode ,js code in html files
  )

(defun samray/toggle-web-indent ()
  "Toggle indent for web-mode."
  (interactive)
  ;; web development
  (if (or (eq major-mode 'js-mode)(eq major-mode 'js2-mode))
      (progn
	(setq js-indent-level (if (= js-indent-level 2) 4 2))
	(setq js2-basic-offset(if (= js2-basic-offset 2) 4 2))))
  (if (eq major-mode 'web-mode)
      (progn
	(setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	(setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	(setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2)  4 2)))
  (setq indent-tabs-mode nil))

;;;----------------;;;
;;;     JS Mode    ;;;
;;;----------------;;;

;; improved Javascript editing mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  )

(use-package nodejs-repl
  :ensure t
  :commands (nodejs-repl
             nodejs-repl-send-buffer
             nodejs-repl-switch-to-repl
             nodejs-repl-send-region
             nodejs-repl-send-last-sexp
             nodejs-repl-execute
             nodejs-repl-load-file)
  )
(use-package js2-refactor
  :defer t
  :ensure t
  :init (add-hook 'js2-mode-hook 'js2-refactor-mode))
;;; Javascript auto-completion in Emacs using js2-mode's parser and Skewer-mode
(use-package ac-js2
  :ensure t
  :defer t
  :init (add-hook 'js2-mode-hook 'ac-js2-mode)
  :config (progn
	    (setq ac-js2-evaluate-calls t)
	    ))

;;; live web development with Emacs
;;; Provides live interaction with Javascript,Css,and Html in a web browser
;;; Usage :
;;; M-x run-skewer to attach a browser to Emacs
;;; From a js2-mode buffer with skewer-mode minor mode enabled, send forms to the browser to evaluate.
(use-package skewer-mode
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'js2-mode-hook 'skewer-mode)
	  (add-hook 'css-mode-hook 'skewer-css-mode)
	  (add-hook 'html-mode-hook 'skewer-html-mode)
	  ))

;;;----------------;;;
;;;   Html Mode    ;;;
;;;----------------;;;

;;; Generate Html and Css code 
(use-package emmet-mode
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
	  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
	  ))

;;;----------------;;;
;;;   Vue Mode    ;;;
;;;----------------;;;

(use-package vue-mode
  :ensure t
  :mode ("\\.vue\\'" . vue-mode)
  :config (progn
	    (setq mmm-submode-decoration-level 0)
	    ))
(provide 'init-web)
;;; init-web.el ends here
