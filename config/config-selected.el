;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; selected.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package selected
  :commands selected-minor-mode selected-global-mode
  :delight selected-minor-mode
  :init
  (setq my:selected-keymap (make-sparse-keymap))
  (selected-global-mode)
  :config
  (bind-key "s" my:selected-keymap selected-keymap)
  :bind
  (:map my:selected-keymap
        ("q" . selected-off)
        ("u" . upcase-region)
        ("d" . downcase-region)))
