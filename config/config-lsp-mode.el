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
    :config
    :custom
    (lsp-ui-peek-always-show t)
    :hook
    ((lsp-mode . lsp-ui-mode)))

  (use-package company-lsp
    :config
    (push 'company-lsp  company-backends))

  (use-package hydra
    :config
    (defhydra hydra-lsp (:hint nil)
"Code Navigation
>"
      ("C-i" completion-at-point          "completion"        :exit t)
      ("."   lsp-ui-peek-find-definitions "peek definitions"  :exit t)
      ("/"   lsp-ui-peek-find-references  "peek references"   :exit t)
      (","   xref-pop-marker-stack        "pop marker"               )
      ("x"   lsp-rename                   "rename"            :exit t)
      ("M-." xref-find-definitions        "find definitions"  :exit t)
      ("M-?" xref-find-references         "find references"   :exit t))
    (bind-key "c" 'hydra-lsp/body keymap-ctrl-meta-space))
  )


