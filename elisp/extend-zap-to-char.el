;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extend-zap-to-char.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

(autoload 'zap-up-to-char "misc" t)

(defvar eztc:printing-char-command-alist
  '((zap-to-char . eztc:zap-to-car)
    (zap-up-to-char . eztc:zap-up-to-char)))

(defvar eztc:printing-char-commond 'zap-up-char)

(defun eztc:zap-to-car (char)
  (eztc:clear)
  (zap-to-char 1 char))

(defun eztc:zap-up-to-char (char)
  (eztc:clear)
  (zap-up-to-char 1 char))

(defun eztc:printing-char ()
  (interactive)
  (let ((func (cdr(assoc eztc:printing-char-commond
                         eztc:printing-char-command-alist))))
    (if func
        (funcall func last-command-event)
      (funcall eztc:printing-char-commond last-command-event))))

(defvar eztc:keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") 'eztc:printing-char)
    (define-key map (kbd "\t") 'eztc:printing-char)
    (dotimes (i (- 256 ?\s))
      (define-key map (vector (+ i ?\s)) 'eztc:printing-char))
    map))

(defvar eztc:position nil)
(defvar eztc:command nil)
(defvar eztc:keep-pred-list '(eztc:isearch-mode-p
                              eztc:ace-jump-mode-p))

;; (defadvice isearch-exit (before eztc:move-to-match-beginning activate)
;;   (when (and eztc:command
;;              (eq this-command 'isearch-exit)
;;              isearch-forward)
;;     (backward-char (length isearch-string))))

(defun eztc:isearch-mode-p ()
  isearch-mode)

(defun eztc:ace-jump-mode-p ()
  (and (featurep 'ace-jump-mode) ace-jump-mode))

(defun eztc:clear ()
  (remove-hook 'post-command-hook 'eztc:kill-region)
  (setq eztc:command nil)
  (setq eztc:position nil))

(defun eztc:kill-region ()
  (when (and (not (eq eztc:command this-command))
             (not (cl-loop for pred in eztc:keep-pred-list
                           if (funcall pred) return it)))
    (let ((p1 eztc:position)
          (p2 (point)))
      (eztc:clear)
      (unless (eq p1 p2)
        (kill-region p1 p2)))))

(defun extend-zap-to-char ()
  (interactive)
  (setq eztc:position (point))
  (setq eztc:command this-command)
  (set-transient-map eztc:keymap)
  (add-hook 'post-command-hook 'eztc:kill-region)
  (message "extend-zap-to-char"))

(provide 'extend-zap-to-char)
