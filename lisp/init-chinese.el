;;; package --- Summary
;;; code:
;;; Commentary:

(use-package youdao-dictionary
  :ensure t
  :commands (youdao-dictionary-search-at-point+ youdao-dictionary-search-from-input)
  :init
  (defun samray/youdao-dictionary-buffer-quit ()
    "Quit youdao-dictionary Buffer and delete window"
    (interactive)
    (kill-this-buffer)
    (delete-window)
    )
  :config (progn
	    (setq url-automatic-caching t)
	    ))

(use-package pyim
  :ensure t
  :config

  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure t
    :config (pyim-basedict-enable))
  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  ;; pyim-probe-program-mode
                  ;; pyim-probe-org-structure-template
  		  ))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 开启拼音搜索功能
  (setq pyim-isearch-enable-pinyin-search t)
  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)
  ;; 选词框显示 5 个候选词
  (setq pyim-page-length 8)
  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-n" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-c C-;" . pyim-delete-word-from-personal-buffer)))

(defun samray/delete-whitespace-between-english-and-chinese-char ()
  "Because the chinese input method i use,i will left whitespace between letter;
and chinese char,so just delete it"
  (interactive)
  (save-restriction
    (if (region-active-p)
        (narrow-to-region (region-beginning) (region-end)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "\\([a-zA-Z0-9]\\)[ ]+\\(\\cc\\)" nil t)
        (replace-match "\\1\\2" t nil))
      (while (search-forward-regexp "\\(\\cc\\)[ ]+\\([a-zA-Z0-9]\\)" nil t)
        (replace-match "\\1\\2" t nil))  ;because i could not figure out the
                                        ;right regexp,so just use such silly way
      )))

;;; org-mode 导出中文的问题解决
;; org-mode export to latex
(with-eval-after-load  'org
  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("samray-org-article"
                 "\\documentclass{article}
\\usepackage[slantfont, boldfont]{xeCJK}
\\usepackage{titlesec}
\\usepackage{hyperref}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
\\parindent 1em
\\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
\\setCJKsansfont{WenQuanYi Micro Hei}
\\setCJKmonofont{WenQuanYi Micro Hei Mono}
\\setmainfont{DejaVu Sans} % 英文衬线字体
\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{DejaVu Sans Mono}
%\\setmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
%\\setsansfont{WenQuanYi Micro Hei}
%\\setmonofont{WenQuanYi Micro Hei Mono}

%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures{Mapping=tex-text}

% [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
\\usepackage{hyperref}
\\hypersetup{
colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
linkcolor=[rgb]{0,0.37,0.53},
citecolor=[rgb]{0,0.47,0.68},
filecolor=[rgb]{0,0.37,0.53},
urlcolor=[rgb]{0,0.37,0.53},
pagebackref=true,
linktoc=all,}
% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

% 代码设置
\\lstset{numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
frame=shadowbox,
breaklines=true,
rulesepcolor= \\color{ red!20!green!20!blue!20}
}
[EXTRA]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

)
;; 在创建 org-mode buffer, 自动添加 latex class 解决生成中文pdf 问题
(eval-after-load 'autoinsert
  '(define-auto-insert '(org-mode . "Chinese Org skeleton")
     '("Description: "
       "#+LATEX_CLASS: samray-org-article" \n
       "#+LATEX_CLASS_OPTIONS: [oneside,A4paper,12pt]" \n)))
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion


(provide 'init-chinese)
;;; init-chinese.el ends here
