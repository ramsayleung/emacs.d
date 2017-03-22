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
(use-package chinese-pyim
  :ensure t
  :config
  ;; 激活 basedict 拼音词库
  (use-package chinese-pyim-basedict
    :ensure t
    :config (chinese-pyim-basedict-enable))

  (setq default-input-method "chinese-pyim")

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
                  pyim-probe-program-mode
                  ;; pyim-probe-org-structure-template
  		  ))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (setq pyim-isearch-enable-pinyin-search t)

  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
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
;;; solve ths issue that org-table cannot indent when english char mix with
;;; chinese char
;;; (set-frame-font "Source Code Pro-11")
(defun samray/handle-org-table-indent-with-chinese ()
  "Deal with  issue that chinese char cannot get along with org-table."
  (interactive)
  (save-excursion
    (progn
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "WenQuanYi Micro Hei")))
      ;; tune rescale so that Chinese character width = 2 * English character width
      (setq face-font-rescale-alist '((samray-current-font. 1.0) ("WenQuanYi" . 1.23)))
      )
    )
  )
(add-hook 'after-init-hook 'samray/handle-org-table-indent-with-chinese)
(add-hook 'after-setting-font-hook 'samray/handle-org-table-indent-with-chinese)

;;; org-mode 导出中文的问题解决
;;org-mode export to latex
(with-eval-after-load  'org
  (require 'ox-latex)
  (setq org-export-latex-listings t)

  ;;org-mode source code setup in exporting to latex
  (add-to-list 'org-latex-listings '("" "listings"))
  (add-to-list 'org-latex-listings '("" "color"))

  (add-to-list 'org-latex-packages-alist
	       '("" "xcolor" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "listings" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "fontspec" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "indentfirst" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "xunicode" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "geometry"))
  (add-to-list 'org-latex-packages-alist
	       '("" "float"))
  (add-to-list 'org-latex-packages-alist
	       '("" "longtable"))
  (add-to-list 'org-latex-packages-alist
	       '("" "tikz"))
  (add-to-list 'org-latex-packages-alist
	       '("" "fancyhdr"))
  (add-to-list 'org-latex-packages-alist
	       '("" "textcomp"))
  (add-to-list 'org-latex-packages-alist
	       '("" "amsmath"))
  (add-to-list 'org-latex-packages-alist
	       '("" "tabularx" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "booktabs" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "grffile" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "wrapfig" t))
  (add-to-list 'org-latex-packages-alist
	       '("normalem" "ulem" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "amssymb" t))
  (add-to-list 'org-latex-packages-alist
	       '("" "capt-of" t))
  (add-to-list 'org-latex-packages-alist
	       '("figuresright" "rotating" t))
  (add-to-list 'org-latex-packages-alist
	       '("Lenny" "fncychap" t))

  (add-to-list 'org-latex-classes
	       '("samray-org-book"
		 "\\documentclass{book}
\\usepackage[slantfont, boldfont]{xeCJK}
% chapter set
\\usepackage{titlesec}
\\usepackage{hyperref}

[NO-DEFAULT-PACKAGES]
[PACKAGES]



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
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("samray-org-article"
                 "\\documentclass{article}
\\usepackage[slantfont, boldfont]{xeCJK}
\\usepackage{titlesec}
\\usepackage{hyperref}

[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\parindent 2em

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

  (add-to-list 'org-latex-classes
               '("samray-org-beamer"
                 "\\documentclass{beamer}
\\usepackage[slantfont, boldfont]{xeCJK}
% beamer set
\\usepackage[none]{hyphenat}
\\usepackage[abs]{overpic}

[NO-DEFAULT-PACKAGES]
[PACKAGES]

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

  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          ;;"biber %b" "xelatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
)
(provide 'init-chinese)
;;; init-chinese.el ends here
