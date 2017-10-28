;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bm.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bm)

(custom-set-variables '(bm-in-lifo-order t))

(defvar my-bm-key-list '(("n" "next" bm-next "j")
                         ("p" "previous" bm-previous "k")
                         ("b" "bookmark" bm-toggle "t")
                         ("D" "remove all" bm-remove-all-current-buffer)))

(defun my-bm-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (dolist (elm my-bm-key-list map)
      (define-key map (kbd (nth 0 elm)) (nth 2 elm))
      (dolist (k (cdddr elm))
        (define-key map (kbd k) (nth 1 elm))))))

(defun my-bm-make-message ()
  (let ((msg ""))
    (dolist (elm my-bm-key-list msg)
      (when (> (length msg) 0)
        (setq msg (concat msg ", ")))
      (setq msg (format "%s[%s] - %s" msg (nth 0 elm) (nth 1 elm))))))

(defmacro my-bm-set-temporary-keymap (func)
  `(defadvice ,func (after set-temporary-keymap activate)
     (set-temporary-overlay-map (my-bm-make-keymap) t)
     (or (minibufferp) (message (my-bm-make-message)))))

(defun my-bm-begin ()
  (interactive)
  ; do nothing
  )

(my-bm-set-temporary-keymap my-bm-begin)
(my-bm-set-temporary-keymap bm-toggle)
(my-bm-set-temporary-keymap bm-next)
(my-bm-set-temporary-keymap bm-previous)

(define-key keymap-ctrl-meta-space (kbd "b") 'my-bm-begin)

(provide 'config-bm)
