;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bm.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bm)

(custom-set-variables '(bm-in-lifo-order t))


(require 'attach-transient-keymap)

(define-key keymap-ctrl-meta-space (kbd "b") 'bm-toggle)

(attach-transientkey:define-keylist
 my-bm-key-list (("n" "next" bm-next "j")
                 ("p" "previous" bm-previous "k")
                 ("b" "bookmark" bm-toggle "t")
                 ("D" "remove all" bm-remove-all-current-buffer)))


(provide 'config-bm)
