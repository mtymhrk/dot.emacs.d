;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; easy-kill.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package easy-kill
  :bind
  ("M-w" . easy-kill)
  ;; C-w に別のコマンドをバインドしているのが原因でキーバインドが正しくされない
  ;; ため、キーバインドを修正する
  (:map easy-kill-base-map
        ("C-w" . easy-kill-region)))
