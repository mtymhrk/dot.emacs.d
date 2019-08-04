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
  (setq  projectile-completion-system 'ivy))

(use-package helm-projectile
  :after (mod-helm)
  :config
  (helm-projectile-on)
  :bind
  (:map keymap-ctrl-meta-space
        ("C-p" . helm-projectile)))

(projectile-global-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

