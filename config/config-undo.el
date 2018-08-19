;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winner

(use-package winner
  :config
  (winner-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; undo-tree.el

(use-package undo-tree
  :delight
  :commands undo-tree-mode global-undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (use-package mod-popwin
    :config
    (mod-popwin:add-display-config
     `(,undo-tree-visualizer-buffer-name :height 0.45 :position bottom :stick t)))
  )

(use-package hydra
  :config
  (defhydra hydra-undo ()
    ("C-z" undo-tree-undo "undo")
    ("SPC" undo-tree-undo "undo")
    ("C-SPC" undo-tree-redo "redo")
    ("t" undo-tree-visualize "tree")
    ("w" winner-undo "window-undo")
    ("C-w" winner-redo "window-redo"))
  (bind-key "C-z" 'hydra-undo/body))
