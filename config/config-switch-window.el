;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch-buffer.el
;;;   http://www.emacswiki.org/emacs-en/download/switch-window.el
;;;   http://d.hatena.ne.jp/tomoya/20100807/1281150227
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (auto-install-from-url "https://raw.github.com/dimitri/switch-window/11ec2487889135d84eae45f25699d03b6fb91d14/switch-window.el")

(eval-when-compile (require 'use-package))

(use-package switch-window
  :custom
  (switch-window-increase 14)
  (switch-window-shortcut-style 'qwerty)
  (switch-window-qwerty-shortcuts '("j" "k" "l" ";" "i" "o" "a" "s" "d" "f" "w" "e"))
  :bind
  ("C-<tab>" . switch-window)
  ("<C-S-iso-lefttab>" . switch-window-then-delete)
  (:map switch-window-extra-map
        ("C-k" . switch-window-mvborder-up)
        ("C-j" . switch-window-mvborder-down)
        ("C-h" . switch-window-mvborder-left)
        ("C-l" . switch-window-mvborder-right)
        ("C-b" . balance-windows))
  :config
  (cl-loop for key in switch-window-qwerty-shortcuts
           do (bind-key key nil switch-window-extra-map)))


(use-package mod-switch-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-switch-window)
