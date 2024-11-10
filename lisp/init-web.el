;;; package --- Summary
;;; code:
;;; Commentary:

;;;----------------;;;
;;;    Web Mode    ;;;
;;;----------------;;;
;;; web-beautify is a formatting package of Html Css and Javascript/Json
;; for Emacs

(use-package php-mode
  :ensure t
  :mode (("\\.php\\'" . php-mode)))

(use-package web-mode
  :ensure t
  :mode (
	 ".erb$"
	 ".phtml$"
	 ".php$"
	 ".[agj]sp$"
	 ".as[cp]x$"
	 ".mustache$"
	 ".djhtml$"
	 )
  :init
  (setq web-mode-extra-snippets
	'(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")))
          ("php" . (("dowhile" . "<?php do { ?>\n\n<?php } while (|); ?>")
                    ("debug" . "<?php error_log(__LINE__); ?>")))
	  ))
  (setq web-mode-extra-auto-pairs
	'(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end")))
	  ))
  )

;;;----------------;;;
;;;   Html Mode    ;;;
;;;----------------;;;

;;; Generate Html and Css code
(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'js2-mode-hook 'emmet-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  )

(message "loading init-web")
(provide 'init-web)

;;; init-web.el ends here
