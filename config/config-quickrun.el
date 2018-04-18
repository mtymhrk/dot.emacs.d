;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  quickrun.el
;;;    http://d.hatena.ne.jp/syohex/20111126/1322291515
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 編集中のスクリプトを簡単に実行できる Vim のプラグインの Emacs 版

;;; (auto-install-from-url "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")

(eval-when-compile (require 'use-package))

(use-package quickrun
  :commands quickrun
  :config
  (use-package mod-popwin
    :config
    (add-popwin-special-display-config '("*quickrun*" :noselect t :stick t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-quickrun)
