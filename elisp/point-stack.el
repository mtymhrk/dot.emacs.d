
(eval-when-compile (require 'cl))

(require 'thingatpt)

(defvar point-stack-undo-list nil)
(make-variable-buffer-local 'point-stack-undo-list)

(defvar point-stack-redo-list nil)
(make-variable-buffer-local 'point-stack-redo-list)

(defun point-stack-make-current-point-info ()
  (list (point) (window-start) (thing-at-point 'line)))

(defun point-stack-same-point-p (x y)
  (equal x y))

(defun point-stack-move-to-point (info)
  (destructuring-bind (pt wst ln) info
    (goto-char pt)
    (set-window-start (selected-window) wst)))

(defun point-stack-push ()
  "Push position."
  (interactive)
  (let ((elm (point-stack-make-current-point-info)))
    (unless (point-stack-same-point-p elm (car point-stack-undo-list))
      (setq point-stack-undo-list (cons elm point-stack-undo-list))
      (message "point-stack: current point is pushd.")))
  (setq point-stack-redo-list nil))

(defun point-stack-pop ()
  "Pop position."
  (interactive)
  (when (and (not (eq last-command 'point-stack-repush))
             (not (eq last-command 'point-stack-pop)))
    (let ((elm (point-stack-make-current-point-info)))
      (unless (point-stack-same-point-p elm (car point-stack-undo-list))
        (setq point-stack-redo-list (cons elm nil)))))
  (if point-stack-undo-list
      (let ((elm (car point-stack-undo-list)))
        (setq point-stack-undo-list (cdr point-stack-undo-list))
        (setq point-stack-redo-list (cons elm point-stack-redo-list))
        (point-stack-move-to-point elm)
        (message "point-stack: poped."))
    (message "point-stack: stack empty.")))

(defun point-stack-repush ()
  "Repush position."
  (interactive)
  (if (and point-stack-redo-list (cdr point-stack-redo-list))
      (progn
        (setq point-stack-undo-list (cons (car point-stack-redo-list)
                                          point-stack-undo-list))
        (setq point-stack-redo-list (cdr point-stack-redo-list))
        (point-stack-move-to-point (car point-stack-redo-list))
        (message "point-stack: repushed."))
    (message "point-stack: stack top reached.")))

(defun point-stack-clear ()
  "Clear point stack."
  (interactive)
  (setq point-stack-undo-list nil)
  (setq point-stack-redo-list nil))

(defun point-stack-display-stack ()
  (interactive)
  (with-output-to-temp-buffer "*point-stack*"
    (dolist (elm (reverse point-stack-redo-list))
      (destructuring-bind (pt wst ln) elm
        (princ (format "%d: %s" (line-number-at-pos pt) ln))))
    (princ "----------\n")
    (dolist (elm point-stack-undo-list)
      (destructuring-bind (pt wst ln) elm
        (princ (format "%d: %s" (line-number-at-pos pt) ln))))))

(provide 'point-stack)
