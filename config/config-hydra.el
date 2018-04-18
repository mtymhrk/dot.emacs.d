;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hydra.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package hydra
  :config
  (defhydra hydra-move-error (global-map "M-g")
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("C-n" next-error "next")
    ("C-p" previous-error "previous")))

(provide 'config-hydra)
