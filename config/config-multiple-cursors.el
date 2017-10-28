;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multiple-cursors.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)

(require 'attach-transient-keymap)

(defun my-mc/begin ()
    (interactive)
  ; nothing to do
    )

(define-key keymap-ctrl-meta-space (kbd "m") 'my-mc/begin)
(define-key keymap-ctrl-meta-space (kbd "C-m") 'mc/mark-all-dwim)

(attach-transientkey:define-keylist
 my-mc/key-list (("n" "next"         mc/mark-next-like-this)
                 ("p" "prev"         mc/mark-previous-like-this)
                 ("m" "more"         mc/mark-more-like-this-extended)
                 ("u" "unmark"       mc/unmark-next-like-this)
                 ("U" "unmark(prev)" mc/unmark-previous-like-this)
                 ("s" "skip"         mc/skip-to-next-like-this)
                 ("S" "skip(prev)"   mc/skip-to-previous-like-this)
                 ("*" "all"          mc/mark-all-like-this)
                 ("d" "all-dwim"     mc/mark-all-like-this-dwim)
                 ("i" "insert-num"   mc/insert-numbers)
                 ("o" "sort"         mc/sort-regions)
                 ("O" "reverse"      mc/reverse-regions)))

(attach-transientkey:attach my-mc/begin my-mc/key-list)

(provide 'config-multiple-cursors)
