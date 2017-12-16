;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-lisp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'config-theme
  '(progn
     ;;; 括弧に色付けするよう設定
     (defvar my-lisp-paren-face 'my-paren-face)
     (push '("(\\|)" . my-lisp-paren-face) lisp-font-lock-keywords-2)
     ))

(defun emacs-lisp-mode-hook--0 ()
  (turn-on-eldoc-mode)
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-hook--0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paredit for emacs-lisp-mode

(eval-after-load 'config-paredit
  '(progn
     (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck for emacs-lisp-mode

(eval-after-load 'config-flycheck
  '(progn
     (defun emacs-lisp-mode-hook--flycheck ()
       (flycheck-mode t))

     (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-hook--flycheck)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1 行の文字数上限可視化する

(with-eval-after-load 'fill-column-indicator
  (defun emacs-lisp-mode-hook--fci ()
    (setq fill-column 80)
    (fci-mode 1))
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-hook--fci))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-emacs-lisp-mode)
