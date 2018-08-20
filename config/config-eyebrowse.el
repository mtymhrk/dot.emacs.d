;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eyebrowse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package eyebrowse
  :commands eyebrowse-mode
  :init
  (eyebrowse-mode 1)
  :custom
  ;; eyebrowse-mode-map にキーをバインドしない
  (eyebrowse-keymap-prefix "")
  :config)

(use-package hydra
  :config
  (defhydra hydra-eyebrowse ()
    ("c" eyebrowse-create-window-config "create")
    ("k" eyebrowse-close-window-config "delete")
    ("p" eyebrowse-prev-window-config "prev")
    ("n" eyebrowse-next-window-config "next")
    ("'" eyebrowse-last-window-config "last")
    ("." eyebrowse-switch-to-window-config "select")
    ("0" eyebrowse-switch-to-window-config-0 "switch to 0")
    ("1" eyebrowse-switch-to-window-config-1 "switch to 1")
    ("2" eyebrowse-switch-to-window-config-2 "switch to 2")
    ("3" eyebrowse-switch-to-window-config-3 "switch to 3")
    ("4" eyebrowse-switch-to-window-config-4 "switch to 4")
    ("5" eyebrowse-switch-to-window-config-5 "switch to 5")
    ("6" eyebrowse-switch-to-window-config-6 "switch to 6")
    ("7" eyebrowse-switch-to-window-config-7 "switch to 7")
    ("8" eyebrowse-switch-to-window-config-8 "switch to 8")
    ("9" eyebrowse-switch-to-window-config-9 "switch to 9"))
  (bind-key "w" 'hydra-eyebrowse/body keymap-ctrl-meta-space))
