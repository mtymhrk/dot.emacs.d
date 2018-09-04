;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'use-package))

(use-package cc-mode
  :defer t
  :config
  (defun my-hook-c-mode-common--0 ()
    (show-paren-mode t)
    (setq indent-tabs-mode nil))

  (defun my-hook-c-mode-common--ff-find-other-file ()
    (cond ((eq major-mode 'c-mode)
           (define-key c-mode-map (kbd "C-c .") 'ff-find-other-file))
          ((eq major-mode 'c++-mode)
           (define-key c++-mode-map (kbd "C-c .") 'ff-find-other-file))))

  (add-hook 'c-mode-common-hook #'my-hook-c-mode-common--0)
  (add-hook 'c-mode-common-hook #'my-hook-c-mode-common--ff-find-other-file)

  (use-package cquery
    :commands my-cquery-enable
    :config
    (setq cquery-executable "cquery")

    (defun my-cquery-enable ()
      (condition-case nil
          (lsp-cquery-enable)
        (error nil)))
    :hook
    ((c-mode . my-cquery-enable)
     (c++-mode . my-cquery-enable)))

  (use-package flycheck
    :hook
    ((c-mode-common . flycheck-mode)))

  (use-package helm-gtags
    :hook
    ((c-mode-common . helm-gtags-mode)))

  (use-package fill-column-indicator
    :init
    (defun my-hook-c-mode-common--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    ((c-mode-common . my-hook-c-mode-common--fci))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

