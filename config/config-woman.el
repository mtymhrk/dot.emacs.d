;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WoMan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package woman
  :config
  (setq woman-use-own-frame nil)

  (use-package mod-popwin
    :config

    ;; popwin for Woman-mode
    (mod-popwin:add-display-config
     '("^\\*WoMan" :regexp t :noselect t :stick t))

    ;; popwin for Man-mode
    (mod-popwin:add-display-config
     '(Man-mode :noselect t :stick t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

