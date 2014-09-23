;; スタートアップ非表示
(setq inhibit-startup-screen t)
 
;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)
;; タブ幅
(custom-set-variables '(tab-width 4))
;; yes or no を y or n に
(fset 'yes-or-no-p 'y-or-n-p)
 
;; language and encoding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;(set-default-coding-systems 'utf-8)
 
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
