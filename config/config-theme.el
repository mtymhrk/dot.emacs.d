;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface my-paren-face
  '((((background dark))
     (:foreground "white"))
    (((background light))
     (:foreground "black")))
  "")

(defface my-brace-face
  '((((background dark))
     (:foreground "white"))
    (((background light))
     (:foreground "black")))
  "")

(defface my-bracket-face
  '((((background dark))
     (:foreground "white"))
    (((background light))
     (:foreground "black")))
  "")


(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "elisp")))

;; (load-theme 'original t)
(load-theme 'atom-one-dark t)

;; region と highlight の face 設定が同一で見づらいため region の face を変更
(custom-theme-set-faces
 'atom-one-dark
 '(region ((t (:background "DarkSlateBlue")))))


(provide 'config-theme)
