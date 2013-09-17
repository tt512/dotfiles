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

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;;; install packages automatically
(require 'cl)
(defvar installing-package-list
  '(
    helm
    auto-complete
    markdown-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))


;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

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

;; theme
(load-theme 'wombat t)

;; language and enciding
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
