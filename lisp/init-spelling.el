;;; init-spelling.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Samray, Leung

;; Author: Ramsay, Leung <ramsayleung@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package langtool
  :ensure t
  :config
  (executable-find "languagetool"
		   (setq langtool-bin (executable-find "languagetool")))

  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
	(let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  (setq langtool-autoshow-message-function
	'langtool-autoshow-detail-popup)
  )
(provide 'init-spelling)
;;; init-spelling.el ends here

