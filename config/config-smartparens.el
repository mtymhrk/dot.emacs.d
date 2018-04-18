;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SmartParens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'use-package))

(use-package smartparens
  :commands smartparens-strict-mode
  :custom
  ;; カッコが自動挿入された際に、ハイライトを行わない
  (sp-highlight-pair-overlay nil)
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (scheme-mode . smartparens-strict-mode)
   (list-mode . smartparens-strict-mode))
  :config
  (use-package smartparens-config)
  (sp-use-paredit-bindings)
  (bind-key "C-<" 'sp-splice-sexp-killing-backward smartparens-mode-map)
  (bind-key "C->" 'sp-splice-sexp-killing-forward smartparens-mode-map))

(provide 'config-smartparens)
