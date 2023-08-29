;;; init-dired.el --- optimize dired                 -*- lexical-binding: t; -*-
;;; package --- Summary
;;; code:
;;; Commentary:

;; Author:  <ramsay@workstation>
;; Keywords:
;; Copyright (C) 2017

(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'classic 'nerd))
  (setq neo-window-width 35)
  (setq-default neo-persist-show t)
  (setq-default neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-show-hidden-files t)
  (setq neo-toggle-window-keep-p t)
  (setq neo-force-change-root t)
  (setq neo-vc-integration '(face char))
  ;; face customizations
  (add-hook 'neotree-mode-hook (lambda()
				 (set-face-attribute 'neo-vc-edited-face nil
						     :foreground "yellow4")
				 (set-face-attribute 'neo-vc-added-face nil
						     :foreground "green4")
				 (set-face-attribute 'neo-vc-removed-face nil
						     :foreground "gray")))
  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t))))
  :bind (([f8] . neotree-toggle)))

(require 'dired-x)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(setq dired-kill-when-opening-new-dired-buffer t)
(when (ramsay/mac-os-p)
  (setq dired-use-ls-dired nil))
(defvar dired-compress-files-alist
  '(("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o")
    ("\\.zip\\'" . "zip %o -r --filesync %i"))
  "Control the compression shell command for `dired-do-compress-to'.
Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD the the
corresponding command.

Within CMD, %i denotes the input file(s), and %o denotes the
output file. %i path(s) are relative, while %o is absolute.")

(setq dired-open-extensions
      '(("mkv" . "vlc")
        ("mp4" . "vlc")
        ("avi" . "vlc")))

;;; From http://oremacs.com/2015/01/12/dired-file-size/
(defun dired-get-size ()
  "Get size of specified file."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))
;;;to get rid of the garbage produced by a LaTeX
(setq dired-garbage-files-regexp
      "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")

;;; prevent Emacs from asking some annoying questions
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-alh")

(message "loading init-dired")
(provide 'init-dired)
;;; init-dired.el ends here
