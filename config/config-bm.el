;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bm.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'use-package))

(use-package bm
  :custom
  (bm-in-lifo-order t)
  :config
  (use-package hydra
    :config
    (defhydra hydra-bm ()
      ("n" bm-next                      "next"              )
      ("p" bm-previous                  "previous"          )
      ("b" bm-toggle                    "bookmark"          )
      ("D" bm-remove-all-current-buffer "remove all"        )
      ("q" nil                          "quit"       :exit t))
    (bind-key "b" 'hydra-bm/body keymap-ctrl-meta-space)))

(provide 'config-bm)
