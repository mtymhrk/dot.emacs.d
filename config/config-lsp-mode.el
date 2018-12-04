;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lsp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package lsp-mode
  :commands lsp-mode
  :config

  (use-package lsp-imenu
    :commands lsp-enable-imenu
    :init
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))

  (use-package lsp-ui
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-doc-max-width (/ (frame-width) 3))
    (lsp-ui-doc-max-height (/ (frame-height) 3))
    :hook
    ((lsp-mode . lsp-ui-mode)))

  (use-package company-lsp
    :config
    (push 'company-lsp  company-backends))

  (use-package hydra
    :config
    (defhydra hydra-xref (:hint nil)
      "
xref
"
      (","   xref-pop-marker-stack "pop marker")
      ("M-." xref-find-definitions "find definitions")
      ("M-?" xref-find-references  "find references")
      ("q"   nil                   "quit")))

  (bind-keys :map keymap-for-code-navigation
             ("C-i" . completion-at-point)
             ("i"   . lsp-ui-imenu)
             ("."   . lsp-ui-peek-find-definitions)
             ("/"   . lsp-ui-peek-find-references)
             (","   . hydra-xref/xref-pop-marker-stack)
             ("x"   . lsp-rename)
             ("M-." . hydra-xref/xref-find-definitions)
             ("M-/" . hydra-xref/xref-find-references))
  )


