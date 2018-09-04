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
             web-beautify-js-buffer))

(use-package web-mode
  :ensure t
  :mode (
	 ("\\.phtml\\'" . web-mode)
	 ("\\.tera\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 )
  :init(progn
	 ))

;;;----------------;;;
;;;     JS Mode    ;;;
;;;----------------;;;

;; improved Javascript editing mode
(use-package js2-mode
  :ensure t
  ;; :mode ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode  t)))
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

(use-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" . rjsx-mode)
  :config (progn

	    (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
	    (with-eval-after-load 'rjsx-mode
	      (setq emmet-expand-jsx-className? t)
	      (define-key rjsx-mode-map "<" nil)
	      (define-key rjsx-mode-map (kbd "C-d") nil)
	      (define-key rjsx-mode-map ">" nil))
	    )
  )

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
	  (add-hook 'js2-mode-hook 'emmet-mode)
	  (add-hook 'rjsx-mode-hook 'emmet-mode)
	  ))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))
(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'php-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)
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
