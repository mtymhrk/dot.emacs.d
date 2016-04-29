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

(load-theme 'original t)

(provide 'config-theme)
