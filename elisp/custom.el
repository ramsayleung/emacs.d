;;; package --- Summary
;;; code:
;;; Commentary:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-minimum-prefix-length 1)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" default)))
 '(eldoc-idle-delay 0.08)
 '(evil-leader/leader "SPC")
 '(evil-want-C-u-scroll t)
 '(expand-region-contract-fast-key "V")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(indent-tabs-mode nil)
 '(magit-diff-use-overlays nil)
 '(nil nil t)
 '(org-agenda-files
   (quote
    ("~/SyncDirectory/Org/todo.org" "~/SyncDirectory/Org/agenda.org")))
 '(org-capture-templates
   (quote
    (("a" "Agenda" entry
      (file "~/SyncDirectory/Org/agenda.org" "Agenda")
      "* TODO %?
:PROPERTIES:

:END:
DEADLINE: %^T 
 %i
")
     ("n" "Note" entry
      (file+headline "~/SyncDirectory/Org/notes.org" "Notes")
      "* Note %?
%T")
     ("l" "Link" entry
      (file+headline "~/SyncDirectory/Org/links.org" "Links")
      "* %? %^L %^g 
%T" :prepend t)
     ("b" "Blog idea" entry
      (file+headline "~/SyncDirectory/Org/blog.org" "Blog Topics:")
      "* %?
%T" :prepend t)
     ("t" "To Do Item" entry
      (file+headline "~/SyncDirectory/Org/todo.org" "To Do Items")
      "* TODO  %?
  %i
" :prepend t)
     ("j" "Journal" entry
      (file+datetree "~/SyncDirectory/Org/journal.org")
      "* %?
Entered on %U
  %i
  %a"))) t)
 '(package-selected-packages
   (quote
    (company-tern org-preview-html org-latex babel clang-format company-c-headers irony irony-mode company-shell highlight-symbol imenu-list diff-hl git-gutter workgroups2 popup project-explorer imenu-anywhere imenus dumb-jump evil-escape keyfreq rainbow-mode rainbow-blocks selectric-mode org-present better-shell virtualenvwrapper gruvbox-theme try org-download company-anaconda company-quickhelp org-bullets rainbow-delimiters markdown-mode wakatime-mode counsel-projectile org-pomodoro company flycheck swiper counsel smartparens expand-region dired+ js2-mode web-mode magit nodejs-repl exec-path-from-shell popwin js2-refactor monokai-theme zenburn-theme dracula-theme molokai-theme use-package)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(wakatime-api-key "df5310e6-9d1f-4cf6-bd9a-fd6a417a6c93")
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(wakatime-python-bin "/usr/bin/python")
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "aquamarine"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cadet blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "rosy brown"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "medium spring green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "DarkGoldenrod4"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
