;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package which-key
  :commands which-key-mode
  :delight
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-show-transient-maps nil)
  :config
  (which-key-setup-side-window-bottom))


