;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  quickrun.el
;;;    http://d.hatena.ne.jp/syohex/20111126/1322291515
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 編集中のスクリプトを簡単に実行できる Vim のプラグインの Emacs 版

;;; (auto-install-from-url "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")

(require 'quickrun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin for quickrun

(eval-after-load 'config-popwin
  '(progn
     ;;; quickrun 実行結果をポップアップで表示
     (add-popwin-special-display-config '("*quickrun*" :noselect t :stick t))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-quickrun)
