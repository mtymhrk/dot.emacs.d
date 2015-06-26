;;; display-line-info.el

;; Auther: Hiroki MOTOYAMA <mtymhrk@gmail.com>
;; Version: 0.0.1


(eval-when-compile (require 'cl))

(defvar dli-display-function
  (if (require 'popup nil t)
      (symbol-function 'popup-tip)
    (lambda (msg) (message "%s" msg))))

(set-default (make-local-variable 'dli-infomation-list) '())

(defun dli-current-line-no ()
  (let ((no (count-lines (point-min) (if (eobp) (point) (1+ (point))))))
    (if (eobp) (1+ no) no)))

(defun dli-make-info-item (inf line-no)
  (cond ((symbolp inf)
         (funcall inf line-no))
        ((listp inf)
         (if (symbol-value (car inf))
             (dli-make-info-item (cdr inf) line-no)
           nil))
        ((stringp inf)
         inf)
        (t nil)))

(defun dli-make-infomation ()
  (let ((inf-list (loop for inf in dli-infomation-list
                        collect
                        (dli-make-info-item inf (dli-current-line-no)))))
    (mapconcat #'identity
               (delq "" (delq nil inf-list))
               "\n")))

(defun dli-display ()
  (interactive)
  (let ((inf (dli-make-infomation)))
    (if (and (stringp inf) (> (length inf) 0))
        (funcall dli-display-function inf)
      (message "dli-display: there is no information to be displayed."))))


;;; dli-display コマンドの実行で popup-fill-string が呼出される場合、その引数
;;; の width を nil に設定する advice。
;;; width が nil でない場合、fill-column によって改行が消されてしまうため、
;;; それを回避する。
(defadvice popup-fill-string (before set-width-of-popup-fill-string-to-nil
                                     activate compile)
  (when (and (eq this-command 'dli-display)
             (> (length (ad-get-args 0)) 1))
    (ad-set-arg 1 nil)))


;;; flymake のエラーと警告のメッセージを表示する
(defun dli-info-flymake-err (line-no)
  (let* ((line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info
                                                           line-no)))
         (menu-data (flymake-make-err-menu-data line-no line-err-info-list)))
    (when menu-data
      (let* ((menu-title (nth 0 menu-data))
             (menu-items (nth 1 menu-data))
             (menu-commands (mapcar (lambda (msg) (format "  %s" (car msg)))
                                    menu-items)))
        (format "Flymake: [%s]\n%s"
                menu-title
                (mapconcat #'identity menu-commands "\n"))))))

;;; flycheck のエラーと警告のメッセージを表示する

(defun dli-info-flycheck-format-error (err)
  (let ((line (flycheck-error-line err))
        (column (flycheck-error-column err))
        (level (let ((l (flycheck-error-level err)))
                 (cond ((eq l 'warning) "Warn ")
                       ((eq l 'error)   "Error")
                       ((eq l 'info)    "Info ")
                       (t (symbol-name l)))))
        (checker (symbol-name (flycheck-error-checker err)))
        (message (flycheck-error-message err)))
    (format "  %s: %s" level message)))

(defun dli-info-flycheck-count-error (errors)
  (let ((ec 0)
        (wc 0)
        (ic 0))
    (when errors
      (dolist (err errors)
        (let ((level (flycheck-error-level err)))
          (cond ((eq level 'error)   (setq ec (+ ec 1)))
                ((eq level 'warning) (setq wc (+ wc 1)))
                ((eq level 'info)    (setq ic (+ ic 1)))))))
    (vector ec wc ic)))

(defun dli-info-flycheck-err (line-no)
  (let* ((errors (flycheck-overlay-errors-in (line-beginning-position)
                                             (line-end-position)))
         (counts (dli-info-flycheck-count-error errors))
         (messages (if errors
                       (mapcar #'dli-info-flycheck-format-error errors)
                     nil)))
    (when messages
      (format "Flycheck: [line %d: %d err, %d warn, %d info]\n%s"
              line-no (elt counts 0) (elt counts 1) (elt counts 2)
              (mapconcat #'identity messages "\n")))))

;;; which-func から得られる関数名を表示する
(defun dli-info-which-func (line-no)
  (let ((name (which-function)))
    (format "which-func:\n  %s" (if name name "???"))))

(setq-default dli-infomation-list
              '((which-func-mode . dli-info-which-func)
                (flymake-mode . dli-info-flymake-err)
                (flycheck-mode . dli-info-flycheck-err)))

(provide 'display-line-info)
