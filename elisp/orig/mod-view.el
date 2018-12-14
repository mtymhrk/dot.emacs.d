;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mod-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'view)

(require 'my-util)

;;; view-mode 時は背景色を変える

(defface mod-view:face '((t ())) "view-mode の背景色を変える face")
(set-face-attribute 'mod-view:face nil
                    :background (my-util:color-darken (face-attribute 'default :background)
                                                      0.2))

(defvar mod-view:overlay nil)
(make-variable-buffer-local 'mod-view:overlay)

(defun mod-view:hook-view-mode--overlay ()
  (unless mod-view:overlay ; 何故か hook が複数回呼ばれるので、作成する overlay
                            ; を一だけにするためにチェック
    (setq mod-view:overlay (make-overlay (point-min) (point-max)))
    (overlay-put mod-view:overlay 'face 'mod-view:face)))

(add-hook 'view-mode-hook 'mod-view:hook-view-mode--overlay)

(defun mod-view:delete-overlay (&rest args)
    (when (and (not view-mode) mod-view:overlay)
      (delete-overlay mod-view:overlay)
      (setq mod-view:overlay nil)))

(advice-add 'view-mode
            :after 'mod-view:delete-overlay)


(provide 'mod-view)
