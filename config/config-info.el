;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package info
  :config

  ;; Info-directory-list の初期設定は環境変数 INFOPATH を元に行われるが、
  ;; exec-path-from-shell による INFOPATH の設定よりも前に Info-directory-list
  ;; が 初期設定されるため、Info-directory-list を未初期化 (nil を設定) するこ
  ;; とで、もう一度初期設定を行なわせる
  (setq Info-directory-list nil))

(provide 'config-info)
