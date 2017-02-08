;;; package --- Summary
;;; Commentary:
;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; cl - Common Lisp Extension

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/elisp")


(require 'init-packages)
(require 'init-evil)
(require 'init-ui)
(require 'init-keybindings)
(require 'init-better-default)
(require 'init-better-editing)
(require 'init-ivy)
(require 'init-org)
(require 'init-web)
(require 'init-python)
(require 'init-elisp)

(setq custom-file (expand-file-name "elisp/custom.el" user-emacs-directory))
(load-file custom-file)
