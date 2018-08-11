;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package projectile
  :delight
  :config
  (setq  projectile-completion-system 'helm))

(use-package helm-projectile
  :after (mod-helm)
  :config
  (helm-projectile-on)
  :bind
  (:map mod-helm:command-keymap
        ("p" . helm-projectile)))

(projectile-global-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

