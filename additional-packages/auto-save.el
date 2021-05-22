;;; auto-save.el --- Auto save files when idle

;; Filename: auto-save.el
;; Description: Auto save files when idle
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2013 ~ 2014, Andy Stewart, all rights reserved.
;; Created: 2013-12-31 00:32:00
;; Version: 0.6
;; Last-Updated: 2018-12-20 12:10:44
;;           By: Andy Stewart
;; URL:
;; Keywords: autosave
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Auto save file when emacs idle
;;

;;; Installation:
;;
;; Clone or download this repository (path of the folder is the `<path-to-auto-save>` used below).
;; In your `~/.emacs`, add the following three lines:
;; (add-to-list 'load-path "<path-to-auto-save>") ; add auto-save to your load-path
;; (require 'auto-save)
;; (auto-save-enable)
;;
;; (setq auto-save-silent t)   ; quietly save
;; (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
;;
;; No need more.

;;; Change log:
;;
;; 2018/12/20
;;      * Don't save buffer when yassnippet or company is active.
;;
;; 2018/12/11
;;      * Do not flash minibuffer when saving automatically.
;;
;; 2018/10/05
;;      * Update font lock before save file.
;;
;; 2018/08/14
;;      *Fixed typo, change `auto-save-slient' to `auto-save-silent'.
;;
;; 2018/07/06
;;      * Add new option `auto-save-delete-trailing-whitespace'.
;;
;; 2014/01/04
;;      * Add new function `auto-save-enable' to enable auto-save in user config file.
;;      * Add options: `auto-save-idle' and `auto-save-silent'.
;;
;; 2008/10/20
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

;;; Code:

(defgroup auto-save nil
  "Auto save file when emacs idle."
  :group 'auto-save)

(defcustom auto-save-idle 1
  "The idle seconds to auto save file."
  :type 'integer
  :group 'auto-save)

(defcustom auto-save-silent nil
  "Nothing to dirty minibuffer if this option is non-nil."
  :type 'boolean
  :group 'auto-save)

(defcustom auto-save-delete-trailing-whitespace nil
  "Delete trailing whitespace when save if this option is non-nil.
Note, this option is non-nil, will delete all training whitespace execpet current line,
avoid delete current indent space when you programming."
  :type 'boolean
  :group 'auto-save)

(defvar auto-save-disable-predicates
  nil "disable auto save in these case.")

;; Emacs' default auto-save is stupid to generate #foo# files!
(setq auto-save-default nil)
(setq create-lockfiles nil)

(defun auto-save-buffers ()
  (interactive)
  (let ((autosave-buffer-list))
    (ignore-errors
      (save-excursion
        (dolist (buf (buffer-list))
          (set-buffer buf)
          (when (and
                 ;; Buffer associate with a filename?
                 (buffer-file-name)
                 ;; Buffer is modifiable?
                 (buffer-modified-p)
                 ;; Yassnippet is not active?
                 (or (not (boundp 'yas--active-snippets))
                     (not yas--active-snippets))
                 ;; Company is not active?
                 (or (not (boundp 'company-candidates))
                     (not company-candidates))
                 ;; tell auto-save don't save
                 (not (seq-some (lambda (predicate)
                                  (funcall predicate))
                                auto-save-disable-predicates)))
            (push (buffer-name) autosave-buffer-list)
            (if auto-save-silent
                ;; `inhibit-message' can shut up Emacs, but we want
                ;; it doesn't clean up echo area during saving
                (with-temp-message ""
                  (let ((inhibit-message t))
                    (basic-save-buffer)))
              (basic-save-buffer))
            ))
        ;; Tell user when auto save files.
        (unless auto-save-silent
          (cond
           ;; It's stupid tell user if nothing to save.
           ((= (length autosave-buffer-list) 1)
            (message "# Saved %s" (car autosave-buffer-list)))
           ((> (length autosave-buffer-list) 1)
            (message "# Saved %d files: %s"
                     (length autosave-buffer-list)
                     (mapconcat 'identity autosave-buffer-list ", ")))))
        ))))

(defun auto-save-delete-trailing-whitespace-except-current-line ()
  (interactive)
  (when auto-save-delete-trailing-whitespace
    (let ((begin (line-beginning-position))
          (end (point)))
      (save-excursion
        (when (< (point-min) begin)
          (save-restriction
            (narrow-to-region (point-min) (1- begin))
            (delete-trailing-whitespace)))
        (when (> (point-max) end)
          (save-restriction
            (narrow-to-region end (point-max))
            (delete-trailing-whitespace)))))))

(defvar auto-save-timer nil)

(defun auto-save-set-timer ()
  "Set the auto-save timer.
Cancel any previous timer."
  (auto-save-cancel-timer)
  (setq auto-save-timer
        (run-with-idle-timer auto-save-idle t 'auto-save-buffers)))

(defun auto-save-cancel-timer ()
  (when auto-save-timer
    (cancel-timer auto-save-timer)
    (setq auto-save-timer nil)))

(defun auto-save-enable ()
  (interactive)
  (auto-save-set-timer)
  (add-hook 'before-save-hook 'auto-save-delete-trailing-whitespace-except-current-line)
  (add-hook 'before-save-hook 'font-lock-flush)
  )

(defun auto-save-disable ()
  (interactive)
  (auto-save-cancel-timer)
  (remove-hook 'before-save-hook 'auto-save-delete-trailing-whitespace-except-current-line)
  (remove-hook 'before-save-hook 'font-lock-flush)
  )

(provide 'auto-save)

;;; auto-save.el ends here
