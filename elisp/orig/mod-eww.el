;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-eww
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eww)
(require 'shr)

(require 'my-util)

;;;

(defface mod-eww:face-pre '((t ())) "EWW の pre 要素の背景色を変える face")
(set-face-attribute 'mod-eww:face-pre nil
                    :background (my-util:color-darken (face-attribute 'default :background)
                                                      0.2))
(defun mod-eww:shr-tag-pre (dom)
  "Overlay と使って <pre> 要素の背景色を変更する"
  (let ((start (point)))
    (shr-tag-pre dom)
    (let ((ovr (make-overlay start (point))))
      (overlay-put ovr 'face 'mod-eww:face-pre))))

(push '(pre . mod-eww:shr-tag-pre) shr-external-rendering-functions)

(provide 'mod-eww)

