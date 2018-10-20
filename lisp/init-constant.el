;;; package --- Summary: -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Define constnat variable for configuration

(defvar samray-current-font "Fantasque Sans Mono-14:weight=medium:slant=italic")

(defvar samray-current-theme 'zenburn)

;;timer for automatically changing themes
(defvar samray--interval-timer nil)

;;table is used to save (time themes) pair for automatically changing themes
;;time should be a string. themes should be a variant , not symbos.
(defvar samray--time-themes-table nil)

(defvar samray-additional-packages-path (expand-file-name "additional-packages" user-emacs-directory))

(provide 'init-constant)
;;; init-constant.el ends here
