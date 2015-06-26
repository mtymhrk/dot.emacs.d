;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-flymake-or-flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-flymake nil t)
(require 'helm-flycheck nil t)

(defun helm-flymake-or-flycheck ()
  (interactive)
  (cond
   ((and (featurep 'flycheck) flycheck-mode)
    (call-interactively 'helm-flycheck))
   ((and (featurep 'flymake) flymake-mode)
    (call-interactively 'helm-flymake))
   (t
    (message "flycheck-mode or flymake-mode are not enabled."))))

(provide 'helm-flymake-or-flycheck)
