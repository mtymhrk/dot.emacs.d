;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ddskk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 参考: http://sheephead.homelinux.org/2010/06/18/1894/

;;; set up for skk

;; (require 'skk)

;; ;; 入力システムをSKKに設定する
;; (set-input-method "japanese-skk")

;; ;; 半角カナモードを使用する
;; (setq skk-use-jisx0201-input-method t)

;; ;; L辞書の場所を指定
;; (setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")

;; ;; migemo との兼ね合いで、検索時に SKK が on になっていると
;; ;; 欝陶しい場合があるので抑制する
;; (setq skk-isearch-mode-enable nil)

;; (global-set-key (kbd "C-o") 'toggle-input-method)

(add-to-list 'load-path (concat user-emacs-directory "elisp/ddskk/lisp"))

(require 'skk-autoloads)

(setq skk-user-directory (concat user-emacs-directory "ddskk/"))

;;; チュートリアルの場所設定
(setq skk-tut-file "~/.emacs.d/elisp/ddskk/etc/SKK.tut")

;;; C-x j で skk-mode
(global-set-key (kbd "C-x C-j") 'skk-mode)

(defun skk-load-hook--0 ()
  ;; コメント行を抜けたら ascii にする
  (require 'context-skk)

  ;; C-M-j でアンドゥ確定
  (define-key skk-j-mode-map (kbd "C-M-j") 'skk-undo-kakutei))

(add-hook 'skk-load-hook 'skk-load-hook--0)

(provide 'config-ddskk)
