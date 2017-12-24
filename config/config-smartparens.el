;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SmartParens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smartparens)
(require 'smartparens-config)

(custom-set-variables
 ;; カッコが自動挿入された際に、ハイライトを行わない
 '(sp-highlight-pair-overlay nil))

;; キーバインドを paredit like に設定
(sp-use-paredit-bindings)
(let ((map smartparens-mode-map))
  (define-key map (kbd "C-<") 'sp-splice-sexp-killing-backward)
  (define-key map (kbd "C->") 'sp-splice-sexp-killing-forward))

(smartparens-global-mode)

;; lisp 系のモードでは strict-mode を有効にする
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'scheme-mode-hook 'smartparens-strict-mode)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)

(provide 'config-smartparens)
