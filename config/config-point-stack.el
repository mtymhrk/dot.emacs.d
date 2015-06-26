;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; point-stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'point-stack)

(defvar point-stack-prefix-key "C-M-@")
(defvar point-stack-keymap-alist
  '(("p" . point-stack-pop)
    ("r" . point-stack-repush)
    ("c" . point-stack-clear)
    ("d" . point-stack-display-stack)))

(global-set-key (kbd "C-@") 'point-stack-push)
(global-unset-key (kbd point-stack-prefix-key))
(dolist (key-cmd point-stack-keymap-alist)
  (global-set-key (kbd (concat point-stack-prefix-key " " (car key-cmd)))
                  (cdr key-cmd)))

(require 'smartrep)
(smartrep-define-key global-map point-stack-prefix-key point-stack-keymap-alist)

(provide 'config-point-stack)
