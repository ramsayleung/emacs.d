;;; init-dired.el --- optimize dired                 -*- lexical-binding: t; -*-
;;; package --- Summary
;;; code:
;;; Commentary:

;; Author:  <samray@workstation>
;; Keywords:
;; Copyright (C) 2017

(require 'dired-x)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(when (samray/mac-os-p)
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

;;; steal from http://kuanyui.github.io/2014/06/21/dired-tutorial-and-essential-configs/
;; reuse dired buffer instead of open a lof of new buffers
(defun samray/dired-find-alternate-file ()
  (interactive)
  (if (file-regular-p (dired-get-filename))
      (dired-find-file)
    (dired-find-alternate-file)))
;;; press Enter to open a buffer
(define-key dired-mode-map (kbd "RET") 'samray/dired-find-alternate-file)
;;; prevent Emacs from asking some annoying questions
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-alh")

(message "loading init-dired")
(provide 'init-dired)
;;; init-dired.el ends here
