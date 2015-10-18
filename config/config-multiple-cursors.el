;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multiple-cursors.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)

(defvar my-mc/prefix-key "M-M")
(defvar my-mc/keybinds `((,my-mc/prefix-key . 'mc/mark-next-like-this)
                         ("n" . 'mc/mark-next-like-this)
                         ("p" . 'mc/mark-previous-like-this)
                         ("m" . 'mc/mark-more-like-this-extended)
                         ("u" . 'mc/unmark-next-like-this)
                         ("U" . 'mc/unmark-previous-like-this)
                         ("s" . 'mc/skip-to-next-like-this)
                         ("S" . 'mc/skip-to-previous-like-this)
                         ("*" . 'mc/mark-all-like-this)
                         ("d" . 'mc/mark-all-like-this-dwim)
                         ("i" . 'mc/insert-numbers)
                         ("o" . 'mc/sort-regions)
                         ("O" . 'mc/reverse-regions)))

(require 'smartrep)
(smartrep-define-key global-map my-mc/prefix-key my-mc/keybinds)

(provide 'config-multiple-cursors)
