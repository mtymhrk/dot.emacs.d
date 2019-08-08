;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package projectile
  :delight
  :config
  (projectile-mode +1)
  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (bind-key "p" 'projectile-command-map keymap-ctrl-meta-space)
  (bind-key "C-p" 'projectile-find-file  keymap-ctrl-meta-space)
  (setq  projectile-completion-system 'ivy))

(projectile-global-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

