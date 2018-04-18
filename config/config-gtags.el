;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Memo: helm-gtags を使用するのではれが gtags とその設定は不要

(eval-when-compile (require 'use-package))

(use-package gtags
  :bind
  (:map gtags-mode-map
        ("M-t" . gtags-find-tag)
        ("M-r" . gtags-find-rtag)
        ("M-s" . gtags-find-symbol)
        ("M-." . gtags-find-tag-from-here)
        ("M-*" . gtags-pop-stack)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-gtags
  :custom
  (helm-gtags-path-style 'relative)
  (helm-gtags-ignore-case t)
  (helm-gtags-auto-update t)
  :bind
  (:map helm-gtags-mode-map
        ("M-t" . helm-gtags-find-tag)
        ("M-r" . helm-gtags-find-rtag)
        ("M-s" . helm-gtags-find-symbol)
        ("M-." . helm-gtags-dwim)
        ("M-g M-p"  . helm-gtags-parse-file)
        ("C-c <" . helm-gtags-previous-history)
        ("C-c >" . helm-gtags-next-history)
        ("M-*" . helm-gtags-pop-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

