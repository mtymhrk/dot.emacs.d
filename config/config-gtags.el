;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Memo: helm-gtags を使用するのではれが gtags とその設定は不要

(require 'gtags)

(let ((map gtags-mode-map))
  (define-key map (kbd "M-t") 'gtags-find-tag)
  (define-key map (kbd "M-r") 'gtags-find-rtag)
  (define-key map (kbd "M-s") 'gtags-find-symbol)
  (define-key map (kbd "M-.") 'gtags-find-tag-from-here)
  (define-key map (kbd "M-*") 'gtags-pop-stack))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-gtags)

(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

(let ((map helm-gtags-mode-map))
  (define-key map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key map (kbd "M-.") 'helm-gtags-dwim)
  (define-key map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key map (kbd "M-*") 'helm-gtags-pop-stack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-gtags)
