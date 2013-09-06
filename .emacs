;;; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;; (cond (window-system
;;     (set-default-font "Monospace-11")
;;     (set-fontset-font (frame-parameter nil 'font)
;;         'japanese-jisx0208
;;         '("MigMix 1P-11"."unicode-bmp"))))

;; スタートアップ非表示
(setq inhibit-startup-screen t)
;; ツールバー非表示
(tool-bar-mode -1)
;; メニューバー非表示
;; (menu-bar-mode -1)
;; スクロールバー非表示
;; (set-scroll-bar-mode nil)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)
;; タブ幅
(custom-set-variables '(tab-width 4))
;; yes or no を y or n に


(fset 'yes-or-no-p 'y-or-n-p)

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))
(require 'mozc)
;; or (load-file "/path/to/mozc.el")
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(setq mozc-candidate-style 'overlay)

;; TeX mode
(setq auto-mode-alist
      (append '(("\\.tex$" . latex-mode)) auto-mode-alist))
(setq tex-default-mode 'latex-mode)
(setq tex-start-commands "\\nonstopmode\\input")
;(setq tex-run-command "ptex2pdf -e -ot '-synctex=1 -interaction=nonstopmode'")
;(setq tex-run-command "ptex2pdf -e -u -ot '-synctex=1 -interaction=nonstopmode'")
;(setq tex-run-command "pdftex -synctex=1 -interaction=nonstopmode")
;(setq tex-run-command "luatex -synctex=1 -interaction=nonstopmode")
;(setq tex-run-command "luajittex -synctex=1 -interaction=nonstopmode")
(setq tex-run-command "xetex -synctex=1 -interaction=nonstopmode")
;(setq latex-run-command "ptex2pdf -l -ot '-synctex=1 -interaction=nonstopmode'")
;(setq latex-run-command "ptex2pdf -l -u -ot '-synctex=1 -interaction=nonstopmode'")
;(setq latex-run-command "pdflatex -synctex=1 -interaction=nonstopmode")
;(setq latex-run-command "lualatex -synctex=1 -interaction=nonstopmode")
;(setq latex-run-command "luajitlatex -synctex=1 -interaction=nonstopmode")
(setq latex-run-command "xelatex -synctex=1 -interaction=nonstopmode")
(setq tex-bibtex-command "pbibtex")
;(setq tex-bibtex-command "upbibtex")
;(setq tex-bibtex-command "bibtex")
;(setq tex-bibtex-command "bibtexu")
(require 'tex-mode)
(defun tex-view ()
  (interactive)
  (tex-send-command "evince" (tex-append tex-print-file ".pdf") " &"))
(defun tex-print (&optional alt)
  (interactive "P")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (tex-send-command "acroread" (tex-append tex-print-file ".pdf") " &"))
(setq tex-compile-commands
      '(("platex -synctex=1 -interaction=nonstopmode %f && dvipdfmx %r" "%f" "%r.pdf")
        ("platex -synctex=1 -interaction=nonstopmode %f && dvips -Ppdf -z -f %r.dvi | convbkmk -g > %r.ps && ps2pdf %r.ps" "%f" "%r.pdf")
        ("uplatex -synctex=1 -interaction=nonstopmode %f && dvipdfmx %r" "%f" "%r.pdf")
        ("uplatex -synctex=1 -interaction=nonstopmode %f && dvips -Ppdf -z -f %r.dvi | convbkmk -u > %r.ps && ps2pdf %r.ps" "%f" "%r.pdf")
        ("pdflatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("lualatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("luajitlatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("xelatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
        ("latexmk %f" "%f" "%r.pdf")
        ("latexmk -e '$latex=q/platex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %f" "%f" "%r.pdf")
        ("latexmk -e '$latex=q/platex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -g > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %f" "%f" "%r.pdf")
        ("latexmk -e '$latex=q/uplatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %f" "%f" "%r.pdf")
        ("latexmk -e '$latex=q/uplatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -u > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %f" "%f" "%r.pdf")
        ("latexmk -e '$pdflatex=q/pdflatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtex %%O %%B/' -e '$makeindex=q/makeindex %%O -o %%D %%S/' -norc -gg -pdf %f" "%f" "%r.pdf")
        ("latexmk -e '$pdflatex=q/lualatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %f" "%f" "%r.pdf")
        ("latexmk -e '$pdflatex=q/luajitlatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %f" "%f" "%r.pdf")
        ("latexmk -e '$pdflatex=q/xelatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -xelatex %f" "%f" "%r.pdf")
        ("pbibtex %r" "%r.aux" "%r.bbl")
        ("upbibtex %r" "%r.aux" "%r.bbl")
        ("bibtex %r" "%r.aux" "%r.bbl")
        ("bibtexu %r" "%r.aux" "%r.bbl")
        ("biber %r" "%r.bcf" "%r.bbl")
        ("mendex %r" "%r.idx" "%r.ind")
        ("makeindex %r" "%r.idx" "%r.ind")
        ("texindy %r" "%r.idx" "%r.ind")
        ((concat "\\doc-view" " \"" (car (split-string (format "%s" (tex-main-file)) "\\.")) ".pdf\"") "%r.pdf")
        ("evince %r.pdf &" "%r.pdf")
        ("okular --unique %r.pdf &" "%r.pdf")
        ("zathura -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %r.pdf &" "%r.pdf")
        ("qpdfview --unique %r.pdf &" "%r.pdf")
        ("pdfviewer %r.pdf &" "%r.pdf")
        ("texworks %r.pdf &" "%r.pdf")
        ("mupdf %r.pdf &" "%r.pdf")
        ("firefox -new-window %r.pdf &" "%r.pdf")
        ("chromium --new-window %r.pdf &" "%r.pdf")
        ("acroread %r.pdf &" "%r.pdf")
        ("pdfopen -viewer ar9-tab %r.pdf &" "%r.pdf")))

(defun evince-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf (tex-main-file))
         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
         (ln (format "%d" (line-number-at-pos)))
         (cmd "fwdevince")
         (args (concat "\"" pf "\" " ln " \"" ctf "\"")))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "fwdevince" nil cmd args))))

(add-hook 'latex-mode-hook
          '(lambda ()
             (define-key latex-mode-map (kbd "C-c e") 'evince-forward-search)))

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)

(defun okular-forward-search ()
  (interactive)
  (let* ((ctf (buffer-file-name))
         (mtf (tex-main-file))
         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
         (ln (format "%d" (line-number-at-pos)))
         (cmd "okular")
         (args (concat "--unique \"file:" pf "#src:" ln " " ctf "\"")))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "okular" nil cmd args))))

(add-hook 'latex-mode-hook
          '(lambda ()
             (define-key latex-mode-map (kbd "C-c o") 'okular-forward-search)))

(defun qpdfview-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf (tex-main-file))
         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
         (ln (format "%d" (line-number-at-pos)))
         (cmd "qpdfview")
         (args (concat "--unique \"" pf "#src:" ctf ":" ln ":0\"")))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "qpdfview" nil cmd args))))

(add-hook 'latex-mode-hook
          '(lambda ()
             (define-key latex-mode-map (kbd "C-c q") 'qpdfview-forward-search)))

(defun pdfviewer-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf (tex-main-file))
         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
         (ln (format "%d" (line-number-at-pos)))
         (cmd "pdfviewer")
         (args (concat "\"file:" pf "#src:" ln " " ctf "\"")))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "pdfviewer" nil cmd args))))

(add-hook 'latex-mode-hook
          '(lambda ()
             (define-key latex-mode-map (kbd "C-c p") 'pdfviewer-forward-search)))

;;
;; RefTeX with TeX mode
;;
(add-hook 'latex-mode-hook 'turn-on-reftex)


;; package
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; helm
(require 'helm-config)
(helm-mode 1)

;; cua-mode の設定
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; ac-math
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode) ; make auto-complete aware of `latex-mode`
(add-to-list 'ac-modes 'LaTeX-mode) ; make auto-complete aware of `latex-mode`

(defun ac-latex-mode-setup () ; add ac-sources to default ac-sources
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources)))

(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)


;; theme
(load-theme 'wombat t)

;;; flymake
(require 'flymake)
(add-hook 'find-file-hook 'flymake-find-file-hook)
;;;;;;latex 
;(defun flymake-tex-init ()
;  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;         (local-dir   (file-name-directory buffer-file-name))
;         (local-file  (file-relative-name
;                       temp-file
;                       local-dir)))
;    (list "platex" (list "-file-line-error" "-interaction=nonstopmode" local-file))))
;(defun flymake-tex-cleanup-custom ()
;  (let* ((base-file-name (file-name-sans-extension (file-name-nondirectory flymake-temp-source-file-name)))
;          (regexp-base-file-name (concat "^" base-file-name "\\.")))
;    (mapcar '(lambda (filename)
;                      (when (string-match regexp-base-file-name filename)
;                         (flymake-safe-delete-file filename)))
;                (split-string (shell-command-to-string "ls"))))
;  (setq flymake-last-change-time nil))
;(push '("\\.tex$" flymake-tex-init flymake-tex-cleanup-custom) flymake-allowed-file-name-masks)
;(add-hook 'yatex-mode-hook 'flymake-mode-1)
; 
; 
;(defun flymake-mode-1 ()
;  (if (not (null buffer-file-name)) (flymake-mode))
;  (local-set-key "\C-cd" 'flymake-display-err-minibuf))
; 
;(defun flymake-display-err-minibuf ()
;  "Displays the error/warning for the current line in the minibuffer"
;  (interactive)
;  (let* ((line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info (flymake-current-line-no))))
;         (count (length line-err-info-list)))
;    (while (> count 0)
;      (when line-err-info-list
;        (let* ((text (flymake-ler-text (nth (1- count) line-err-info-list)))
;               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
;          (message "[%s] %s" line text)))
;      (setq count (1- count)))))
;;;
;;; AUCTeX
;;;
;(setq japanese-LaTeX-default-style "jsarticle")
;(setq TeX-engine-alist '((ptex "pTeX" "ptex2pdf -e -ot '%S %(mode)'" "ptex2pdf -l -ot '%S %(mode)'" "eptex")
;                         (uptex "upTeX" "ptex2pdf -e -u -ot '%S %(mode)'" "ptex2pdf -l -u -ot '%S %(mode)'" "euptex")))
;(setq TeX-engine 'ptex)
;;(setq TeX-engine 'uptex)
;;(setq TeX-engine 'luatex)
;;(setq TeX-engine 'xetex)
;(setq TeX-view-program-selection '((output-pdf "Evince")))
;;(setq TeX-view-program-selection '((output-pdf "Okular")))
;(setq preview-image-type 'dvipng)
;(setq TeX-source-correlate-method 'synctex)
;(setq TeX-source-correlate-start-server t)
;(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;(add-hook 'LaTeX-mode-hook
;  (function (lambda ()
;  (add-to-list 'TeX-command-list
;   '("pdfpLaTeX" "platex %S %(mode) %t && dvipdfmx %d"
;     TeX-run-TeX nil (latex-mode) :help "Run pLaTeX and dvipdfmx"))
;  (add-to-list 'TeX-command-list
;   '("pdfpLaTeX2" "platex %S %(mode) %t && dvips -Ppdf -z -f %d | convbkmk -g > %f && ps2pdf %f"
;     TeX-run-TeX nil (latex-mode) :help "Run pLaTeX, dvips, and ps2pdf"))
;  (add-to-list 'TeX-command-list
;   '("pdfupLaTeX" "uplatex %S %(mode) %t && dvipdfmx %d"
;     TeX-run-TeX nil (latex-mode) :help "Run upLaTeX and dvipdfmx"))
;  (add-to-list 'TeX-command-list
;   '("pdfupLaTeX2" "uplatex %S %(mode) %t && dvips -Ppdf -z -f %d | convbkmk -u > %f && ps2pdf %f"
;     TeX-run-TeX nil (latex-mode) :help "Run upLaTeX, dvips, and ps2pdf"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk" "latexmk %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-pdfpLaTeX" "latexmk -e '$latex=q/platex %%O %S %(mode) %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfpLaTeX"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-pdfpLaTeX2" "latexmk -e '$latex=q/platex %%O %S %(mode) %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -g > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfpLaTeX2"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-pdfupLaTeX" "latexmk -e '$latex=q/uplatex %%O %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfupLaTeX"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-pdfupLaTeX2" "latexmk -e '$latex=q/uplatex %%O %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -u > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfupLaTeX2"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-pdfLaTeX" "latexmk -e '$pdflatex=q/pdflatex %%O %S %(mode) %%S/' -e '$bibtex=q/bibtex %%O %%B/' -e '$makeindex=q/makeindex %%O -o %%D %%S/' -norc -gg -pdf %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfLaTeX"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-LuaLaTeX" "latexmk -e '$pdflatex=q/lualatex %%O %S %(mode) %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-LuaJITLaTeX" "latexmk -e '$pdflatex=q/luajitlatex %%O %S %(mode) %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaJITLaTeX"))
;  (add-to-list 'TeX-command-list
;   '("Latexmk-XeLaTeX" "latexmk -e '$pdflatex=q/xelatex %%O %S %(mode) %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -xelatex %t"
;     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-XeLaTeX"))
;  (add-to-list 'TeX-command-list
;   '("pBibTeX" "pbibtex %s"
;     TeX-run-BibTeX nil t :help "Run pBibTeX"))
;  (add-to-list 'TeX-command-list
;   '("upBibTeX" "upbibtex %s"
;     TeX-run-BibTeX nil t :help "Run upBibTeX"))
;  (add-to-list 'TeX-command-list
;   '("BibTeXu" "bibtexu %s"
;     TeX-run-BibTeX nil t :help "Run BibTeXu"))
;  (add-to-list 'TeX-command-list
;   '("Mendex" "mendex %s"
;     TeX-run-command nil t :help "Create index file with mendex"))
;  (add-to-list 'TeX-command-list
;   '("Evince" "evince %s.pdf"
;     TeX-run-discard-or-function t t :help "Run Evince"))
;  (add-to-list 'TeX-command-list
;   '("fwdevince" "fwdevince %s.pdf %n \"%b\""
;     TeX-run-discard-or-function t t :help "Forward search with Evince"))
;  (add-to-list 'TeX-command-list
;   '("Okular" "okular --unique \"file:\"%s.pdf\"#src:%n %a\""
;     TeX-run-discard-or-function t t :help "Forward search with Okular"))
;  (add-to-list 'TeX-command-list
;   '("zathura" "zathura -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %s.pdf"
;     TeX-run-discard-or-function t t :help "Run zathura"))
;  (add-to-list 'TeX-command-list
;   '("qpdfview" "qpdfview --unique \"\"%s.pdf\"#src:%b:%n:0\""
;     TeX-run-discard-or-function t t :help "Forward search with qpdfview"))
;  (add-to-list 'TeX-command-list
;   '("PdfViewer" "pdfviewer \"file:\"%s.pdf\"#src:%n %b\""
;     TeX-run-discard-or-function t t :help "Forward search with PdfViewer"))
;  (add-to-list 'TeX-command-list
;   '("TeXworks" "texworks %s.pdf"
;     TeX-run-discard-or-function t t :help "Run TeXworks"))
;  (add-to-list 'TeX-command-list
;   '("MuPDF" "mupdf %s.pdf"
;     TeX-run-discard-or-function t t :help "Run MuPDF"))
;  (add-to-list 'TeX-command-list
;   '("Firefox" "firefox -new-window %s.pdf"
;     TeX-run-discard-or-function t t :help "Run Mozilla Firefox"))
;  (add-to-list 'TeX-command-list
;   '("Chromium" "chromium --new-window %s.pdf"
;     TeX-run-discard-or-function t t :help "Run Chromium"))
;  (add-to-list 'TeX-command-list
;   '("acroread" "acroread %s.pdf"
;     TeX-run-discard-or-function t t :help "Run Adobe Reader"))
;  (add-to-list 'TeX-command-list
;   '("pdfopen" "pdfopen -viewer ar9-tab %s.pdf"
;     TeX-run-discard-or-function t t :help "Run Adobe Reader")))))
; 
;;;
;;; RefTeX with AUCTeX
;;;
;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;(setq reftex-plug-into-AUCTeX t)
; 
;;;
;;; kinsoku.el
;;;
;(setq kinsoku-limit 10)
