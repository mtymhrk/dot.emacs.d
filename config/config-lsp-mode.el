;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lsp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package lsp-mode
  :commands lsp-mode
  :bind
  (:map keymap-ctrl-meta-space
        ("c C-i" . completion-at-point)
        ("c ." . xref-find-definitions)
        ("c ?" . xref-find-references)
        ("c x" . lsp-rename))

  :config

  (use-package lsp-imenu
    :commands lsp-enable-imenu
    :init
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))

  (use-package company-lsp
    :config
    (push 'company-lsp  company-backends))
  )
