;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  volatile-highlits.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; yank された文字や undo で戻された文字をハイライトする elisp

(eval-when-compile (require 'use-package))

(use-package volatile-highlights
  :commands volatile-highlights-mode
  :delight
  :init
  (volatile-highlights-mode)
  :config
  ;; ハイライト時の face 設定
  (set-face-background 'vhl/default-face "dark slate blue"))



