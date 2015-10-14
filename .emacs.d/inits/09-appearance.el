(tool-bar-mode -1)        ; ツールバー非表示
(menu-bar-mode -1)        ; メニューバー非表示
(set-scroll-bar-mode nil) ; スクロールバー非表示

; 対応する括弧をハイライト
(show-paren-mode 1)
 
;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.90))
 
;; 折り返さない
;(setq-default truncate-lines 1)
 
;(set-frame-font "TakaoGothic-11")

;; Font setting
(cond ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil
                           :family "Source Code Pro"
                           :height 100
                           :weight 'normal)
       (set-fontset-font nil 'japanese-jisx0208
                         (font-spec :family "Source Han Sans JP")))

      ((eq system-type 'darwin)
       (set-face-attribute 'default nil
                           :family "Menlo"
                           :height 110)
       (set-fontset-font nil 'japanese-jisx0208
                         (font-spec :family "Hiragino Kaku Gothic ProN"))))

; theme
(load-theme 'wombat t)
