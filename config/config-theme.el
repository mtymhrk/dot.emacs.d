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


(defvar atom-one-dark-colors-alist
  '(("atom-one-dark-accent"   . "#528BFF")
    ("atom-one-dark-fg"       . "#ABB2BF")
    ;; ("atom-one-dark-bg"       . "#282C34")
    ("atom-one-dark-bg"       . "#20222d")
    ("atom-one-dark-bg-1"     . "#121417")
    ("atom-one-dark-bg-hl"    . "#2C323C")
    ("atom-one-dark-gutter"   . "#4B5363")
    ("atom-one-dark-mono-1"   . "#ABB2BF")
    ("atom-one-dark-mono-2"   . "#828997")
    ("atom-one-dark-mono-3"   . "#5C6370")
    ("atom-one-dark-cyan"     . "#56B6C2")
    ("atom-one-dark-blue"     . "#61AFEF")
    ("atom-one-dark-purple"   . "#C678DD")
    ("atom-one-dark-green"    . "#98C379")
    ("atom-one-dark-red-1"    . "#E06C75")
    ("atom-one-dark-red-2"    . "#BE5046")
    ("atom-one-dark-orange-1" . "#D19A66")
    ("atom-one-dark-orange-2" . "#E5C07B")
    ("atom-one-dark-gray"     . "#3E4451")
    ("atom-one-dark-silver"   . "#9DA5B4")
    ("atom-one-dark-black"    . "#21252B")
    ("atom-one-dark-border"   . "#181A1F"))
  "List of Atom One Dark colors.")

;; (load-theme 'original t)
(load-theme 'atom-one-dark t)

;; region と highlight の face 設定が同一で見づらいため region の face を変更
(custom-theme-set-faces
 'atom-one-dark
 '(region ((t (:background "DarkSlateBlue")))))


(provide 'config-theme)
