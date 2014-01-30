;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)
;; タブ幅
(custom-set-variables '(tab-width 4))
;; yes or no を y or n に
(fset 'yes-or-no-p 'y-or-n-p)


;; クリップボードを使う
(setq x-select-enable-clipboard t)
(global-set-key "\C-y" 'x-clipboard-yank)



;; theme
(load-theme 'wombat t)

;; language and encoding
;(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
