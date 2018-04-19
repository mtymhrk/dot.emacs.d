;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SmartParens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'use-package))

(use-package smartparens
  :commands smartparens-global-mode smartparens-mode smartparens-strict-mode
  :init
  (smartparens-global-mode)
  :custom
  ;; カッコが自動挿入された際に、ハイライトを行わない
  (sp-highlight-pair-overlay nil)
  :config
  (use-package smartparens-config)
  (sp-use-paredit-bindings)
  (bind-key "C-<" 'sp-splice-sexp-killing-backward smartparens-mode-map)
  (bind-key "C->" 'sp-splice-sexp-killing-forward smartparens-mode-map))


