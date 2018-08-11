;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-lisp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package elisp-mode
  :commands emacs-lisp-mode
  :delight emacs-lisp-mode "ELisp"
  :config
  (defun my-hook-emacs-lisp-mode--0 ()
    (setq indent-tabs-mode nil))

  (add-hook 'emacs-lisp-mode-hook #'my-hook-emacs-lisp-mode--0)

  (use-package smartparens
    :hook
    ((emacs-lisp-mode . smartparens-strict-mode)))

  (use-package flycheck
    :hook
    ((emacs-lisp-mode . flycheck-mode)))

  (use-package fill-column-indicator
    :init
    (defun my-hook-emacs-lisp-mode--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    ((emacs-lisp-mode . my-hook-emacs-lisp-mode--fci))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

