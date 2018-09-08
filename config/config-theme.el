;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :defer t
  :config
  ;; doom-modeline が eldoc-in-minibuffer-mode を有効にするが、不要なのと、これ
  ;; を有効にすると eval-expression で C-h が効かなくなるで無効にする
  (when eldoc-in-minibuffer-mode
    (eldoc-in-minibuffer-mode -1))
  :hook
  (after-init . doom-modeline-init))
