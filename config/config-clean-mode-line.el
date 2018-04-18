;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clean-mode-line
;;;    http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; mode-line のメジャーモード、マイナーモードの表示をスリム化する

(eval-when-compile (require 'use-package))

(use-package clean-mode-line
  :config
  (setq mode-line-cleaner-alist
        `((yas-minor-mode . " YS")
          (abbrev-mode . "")
          (gtags-mode . " GT")
          (undo-tree-mode . " UT")
          (helm-mode . " Hm")
          (helm-gtags-mode . "")
          (flymake-mode . " FM")
          (global-whitespace-mode . "")
          (whitespace-mode . "")
          (volatile-highlights-mode . "")
          (auto-highlight-symbol-mode . "")
          (ruby-block-mode . "")
          (ruby-end-mode . "")
          (auto-fill-function . " AF")
          (jaspace-mode . "")
          (paredit-mode . " PE")
          (projectile-mode . "")
          (smooth-scroll-mode . "")
          (eldoc-mode . "")
          (owdriver-mode . "")
          ;; Major modes
          (lisp-interaction-mode . "λ")
          (emacs-lisp-mode . "ELisp"))))

(provide 'config-clean-mode-line)
