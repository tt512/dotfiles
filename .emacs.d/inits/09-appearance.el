(tool-bar-mode -1)        ; ツールバー非表示
(menu-bar-mode -1)        ; メニューバー非表示
(set-scroll-bar-mode nil) ; スクロールバー非表示

; 対応する括弧をハイライト
(show-paren-mode 1)
 
;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.90))
 
;; 折り返さない
;(setq-default truncate-lines 1)
 
;; (cond (window-system
;;     (set-default-font "Monospace-11")
;;     (set-fontset-font (frame-parameter nil 'font)
;;         'japanese-jisx0208
;;         '("MigMix 1P-11"."unicode-bmp"))))
 
;(set-frame-font "TakaoGothic-11")
;(set-frame-font "Ricty-11")

;; Font setting
(cond ((eq system-type 'linux)
       (set-face-attribute 'default nil
                           :family "Ricty"
                           :height 110
                           :weight 'normal))

      ((eq system-type 'darwin)
       (set-face-attribute 'default nil
                           :family "Menlo"
                           :height 110)
       (set-fontset-font nil 'japanese-jisx0208
                         (font-spec :family "Hiragino Kaku Gothic ProN"))))

; theme
(load-theme 'wombat t)
