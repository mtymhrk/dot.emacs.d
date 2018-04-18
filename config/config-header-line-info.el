;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; header-line-info.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; header-line に vcprompt の出力と Flymake の情報、which-func の情報を表示す
;;; るマイナーモード

(eval-when-compile (require 'use-package))

(use-package header-line-info
  :commands global-header-line-info-mode
  :init
  ;; hli-enable-modes に含まれるメジャーモードでは header-line-info-mode を
  ;; on にする
  (global-header-line-info-mode t)
  :config
  ;; vcprompt コマンドの引数 (ブランチ名とパッチキュー名だけを表示)
  (setq hli-vcprompt-cmd-args '("-f" "%b:%q"))
  :bind
  ;; キーバインド
  ("M-H" . hli-toggle-header-line-info-mode))


