;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; irony
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package irony
  :commands irony-mode
  :delight

  :config
  (use-package flycheck-irony
    :hook
    ((flycheck-mode . flycheck-irony-setup)))

  :hook
  ((c++-mode . irony-mode)
   (c-mode . irony-mode)
   (irony-mode . irony-cdb-autosetup-compile-options)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

