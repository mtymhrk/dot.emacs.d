;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elscreen
;;;   http://www.morishima.net/~naoto/software/elscreen/index.php.ja
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package elscreen
  :custom
  ;; タブを表示しない
  (elscreen-display-tab nil)
  ;; elscreen-map をバインドしない
  (elscreen-prefix-key "")
  :config
  (define-key keymap-ctrl-meta-space (kbd "t") elscreen-map)
  ;; elscreen を有効化
  (elscreen-start))

(with-eval-after-load 'wl
  (use-package elscreen-wl))

(use-package mod-elscreen
  :config
  ;; elscreen の mode-line 更新関数を自前のものに差し替える
  (mod-elscreen:enable-colored-elscreen-mode-line)

  ;; デフォルトの window 構成を変更
  (setq mod-elscreen:default-window-config-func #'my-default-window-split)
  (mod-elscreen:enable-default-window-configuration-change)
  )


(use-package helm-elscreen
  :bind (:map elscreen-map
              ("t" . helm-elscreen)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

