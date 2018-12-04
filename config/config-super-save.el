;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; super-save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5)
  :config
  (super-save-mode +1))
