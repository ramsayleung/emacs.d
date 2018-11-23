;;; package --- summary
;;; code:
;;; Commentary:

;;;major mode for markdown
(use-package markdown-mode
  :ensure t
;;; major mode for editing GitHub Flavored Markdown files
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :init (progn
	  (setq markdown-fontify-code-blocks-natively t)
	  (add-hook 'markdown-mode-hook
		    (lambda ()
		      (when buffer-file-name
			(add-hook 'after-save-hook 'check-parens nil t))))))

(message "loading init-markdown")
(provide 'init-markdown)

;;; init-markdown.el ends here
