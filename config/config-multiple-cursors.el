;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multiple-cursors.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'use-package))

(use-package multiple-cursors
  :config
  (use-package hydra
    :config
    (defhydra hydra-multiple-cursors ()
      ("n" mc/mark-next-like-this          "next"                )
      ("p" mc/mark-previous-like-this      "prev"                )
      ("m" mc/mark-more-like-this-extended "more"                )
      ("u" mc/unmark-next-like-this        "unmark"              )
      ("U" mc/unmark-previous-like-this    "unmark(prev)"        )
      ("s" mc/skip-to-next-like-this       "skip"                )
      ("S" mc/skip-to-previous-like-this   "skip(prev)"          )
      ("*" mc/mark-all-like-this           "all"                 )
      ("d" mc/mark-all-like-this-dwim      "all-dwim"            )
      ("i" mc/insert-numbers               "insert-num"          )
      ("o" mc/sort-regions                 "sort"                )
      ("O" mc/reverse-regions              "reverse"             )
      ("q" nil                             "done"         :eixt t))
    (bind-key "m" 'hydra-multiple-cursors/body keymap-ctrl-meta-space)))

(provide 'config-multiple-cursors)
