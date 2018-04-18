;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package company
  :commands company-mode global-company-mode
  :custom
  (company-idle-delay nil)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  :bind
  (:map company-mode-map
        ("M-i" . company-complete))
  (:map company-active-map
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous)
        ("M-i" . company-complete-common)
        ("M-m" . company-complete-selection)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-h" . nil))
  :config
  (setq-default company-backends
                '((company-capf company-files company-keywords company-dabbrev-code company-dabbrev))))

(use-package mod-company)

(global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company-irony

(use-package company-irony
  :config
  (add-to-list 'company-backends 'company-irony))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quickhelp

(use-package company-quickhelp)
(company-quickhelp-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

