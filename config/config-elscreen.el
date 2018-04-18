;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elscreen
;;;   http://www.morishima.net/~naoto/software/elscreen/index.php.ja
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package elscreen
  :config
  ;; タブを表示しない
  (setq elscreen-display-tab nil)

  ;; (setq elscreen-tab-width 12)
  ;; (setq elscreen-tab-display-kill-screen nil)

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
              ("C-z" . helm-elscreen)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-elscreen)
