;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  volatile-highlits.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; yank された文字や undo で戻された文字をハイライトする elisp

;;; (auto-install-from-emacswiki "volatile-highlits.el")

(require 'volatile-highlights)

(volatile-highlights-mode)

;;; ハイライト時の face 設定
(set-face-background 'vhl/default-face "dark slate blue")


(provide 'config-volatile-highlights)
