;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; header-line-info.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; header-line に vcprompt の出力と Flymake の情報、which-func の情報を表示す
;;; るマイナーモード

(require 'header-line-info)

;;; vcprompt コマンドの引数 (ブランチ名とパッチキュー名だけを表示)
(setq hli-vcprompt-cmd-args '("-f" "%b:%q"))

;;; hli-enable-modes に含まれるメジャーモードでは header-line-info-mode を
;;; on にする
(global-header-line-info-mode t)

;;; キーバインド
(global-set-key (kbd "M-H") 'hli-toggle-header-line-info-mode)

(provide 'config-header-line-info)
