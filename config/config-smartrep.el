;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  smartrep.el
;;;    http://d.hatena.ne.jp/m2ym/20110120/1295524932
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; prefix キー付きのキーバインドの連続入力を楽にする elisp

;;; (auto-install-from-url "https://raw.github.com/myuhe/smartrep.el/master/smartrep.el")

(require 'smartrep)

;;; smartrep を使い続けていると 入力したキーのエコー表示が minibuffer にたまり、
;;; minibuffer の表示欄が大きくなってしまうので、キーストロークのエコーを off
;;; にする。
(defadvice smartrep-read-event-loop (around do-not-echo-keystrokes activate)
  (let ((echo-keystrokes 0))
    ad-do-it))

(smartrep-define-key global-map "M-g"
  '(("n" . 'next-error)
    ("p" . 'previous-error)
    ("C-n" . 'next-error)
    ("C-p" . 'previous-error)))

(provide 'config-smartrep)
