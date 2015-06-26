;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popup-keybindings
;;;  http://d.hatena.ne.jp/tequilasunset/20101211/p1
;;;  https://gist.github.com/736979#file_popup_keybindings.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; コマンドを途中までタイプしてすこし待っていると，候補を勝手にポップアッ
;;; プする

(require 'popup-keybindings)

;;; ポップアップするまでの遅延時間
(setq popup-kbs-tip-delay 0.5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs 24.4 で使用すると問題が出るのでその対処

(when (version<= "24.4" emacs-version)
  (fset 'orig-buffer-substring-no-properties
        (symbol-function 'buffer-substring-no-properties))

  (defadvice popup-kbs-collect-kbs-inner
      (around chg--buffer-substring-no-properties activate)
    (unwind-protect
        (progn
          (defun buffer-substring-no-properties (start end)
            (orig-buffer-substring-no-properties start
                                                 (if (>= end start)
                                                     end
                                                   start)))
          ad-do-it)
      (fset 'buffer-substring-no-properties
            (symbol-function 'orig-buffer-substring-no-properties)))))


(provide 'config-popup-keybindings)
