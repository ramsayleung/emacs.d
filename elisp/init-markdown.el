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
	  (setq markdown-command "/usr/local/bin/pandoc")
	  (add-hook 'markdown-mode-hook
		    (lambda ()
		      (when buffer-file-name
			(add-hook 'after-save-hook
				  'check-parens
				  nil t))))))

(use-package pandoc-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  )

(use-package markdown-preview-mode
  :ensure t
  :commands (markdown-mode))

;;; preview markdown file on the fly on my browser
(use-package flymd
  :commands markdown-mode
  :ensure t)

(provide 'init-markdown)

;;; init-markdown.el ends here
