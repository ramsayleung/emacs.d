;;; init-dired.el --- optimize dired                 -*- lexical-binding: t; -*-
;;; package --- Summary
;;; code:
;;; Commentary:

;; Author:  <ramsay@workstation>
;; Keywords:
;; Copyright (C) 2017

(require 'dired-x)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(setq dired-kill-when-opening-new-dired-buffer t)
(when (ramsay/mac-os-p)
  (setq dired-use-ls-dired nil))

;;;to get rid of the garbage produced by a LaTeX
(setq dired-garbage-files-regexp
      "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")

;;; prevent Emacs from asking some annoying questions
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-alh")

(message "loading init-dired")
(provide 'init-dired)
;;; init-dired.el ends here
