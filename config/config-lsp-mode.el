;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lsp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package lsp-mode
  :commands lsp
  :custom
  ;; flyamke ではなく flycheck を使用する
  (lsp-prefer-flymake nil)

  :config

  ;; lsp-auto-configure が t (デフォルト) なら自動的に lsp-ui, lsp-imenu,
  ;; company-lsp を有効にしてくれるので設定をコメントアウト

  ;; (use-package lsp-imenu
  ;;   :commands lsp-enable-imenu
  ;;   :init
  ;;   (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))

  (use-package lsp-ui
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-doc-max-width (/ (frame-width) 3))
    (lsp-ui-doc-max-height (/ (frame-height) 3))
    ;; :hook
    ;; ((lsp-mode . lsp-ui-mode))
    )

  ;; (use-package company-lsp
  ;;   :config
  ;;   (push 'company-lsp  company-backends))

  (use-package hydra
    :config
    (defhydra hydra-xref (:hint nil)
"
^Find^                     ^Pop^               ^Quit^
^^^^^^^^-----------------------------------------------------------------
_M-._: find definitions    _,_: pop            _q_: quit
_M-/_: find references
"
      (","   xref-pop-marker-stack)
      ("M-." xref-find-definitions)
      ("M-/" xref-find-references)
      ("q"   nil)))

  (bind-keys :map keymap-for-code-navigation
             ("C-i" . completion-at-point)
             ("i"   . lsp-ui-imenu)
             ("."   . lsp-ui-peek-find-definitions)
             ("/"   . lsp-ui-peek-find-references)
             (","   . hydra-xref/xref-pop-marker-stack)
             ("x"   . lsp-rename)
             ("d"   . lsp-describe-thing-at-point)
             ("M-." . hydra-xref/xref-find-definitions)
             ("M-/" . hydra-xref/xref-find-references))
  )


