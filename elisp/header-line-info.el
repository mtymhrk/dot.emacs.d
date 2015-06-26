;;; header-line-info.el

;; Auther: Hiroki MOTOYAMA <mtymhrk@gmail.com>
;; Version: 0.0.1


(eval-when-compile (require 'cl))

(defgroup header-line-info nil
  ""
  :group 'convenience)

(defcustom hli-fmtstr-info-item "[%s]"
  ""
  :group 'header-line-info
  :type 'string)

(defcustom hli-default-string "---"
  ""
  :group 'header-line-info
  :type 'string)

(defcustom hli-delimiter " "
  ""
  :group 'header-line-info
  :type 'string)

(defcustom hli-enable-modes
  '(org-mode
    sh-mode
    c++-mode
    c-mode
    emacs-lisp-mode
    lisp-mode
    common-lisp-mode
    scheme-mode
    makefile-mode
    perl-mode
    ruby-mode
    python-mode
    text-mode)
  ""
  :group 'header-line-info
  :type '(repeat string))

(defface hli-face-header-line
  '((((background dark))
     (:foreground "gray80" :background "gray30"))
    (((background light))
     (:foreground "gray80" :background "gray30")))
  ""
  :group 'header-line-info)

(defvar hli-header-line-format nil)
(make-variable-buffer-local 'hli-header-line-format)
(put 'hli-header-line-format 'risky-local-variable t)

(defvar hli-information-list nil)

(defvar hli-orig-header-line-format nil)
(make-variable-buffer-local 'hli-orig-header-line-format)

(defun hli-make-info (enable-func disable-func)
  `((enable . ,enable-func) (disable . ,disable-func)))

(defun hli-call-func (info sym &rest args)
  (let ((func (assoc-default sym (if (symbolp info)
                                     (symbol-value info)
                                   info))))
    (if func
        (apply func args))))

(defun hli-escape-% (str)
  (replace-regexp-in-string "%" "%%" str))

(defun hli-format-info-item (str)
  (hli-escape-% (format hli-fmtstr-info-item
                        (if str str hli-default-string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-func
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface hli-face-which-func
  '((((background dark))
     (:inherit hli-face-header-line))
    (((background light))
     (:inherit hli-face-header-line)))
  ""
  :group 'header-line-info)

(defvar hli-information-which-func
  (hli-make-info 'hli-which-func-enable 'hli-flymake-disable))

(defvar hli-which-func-header-line-format
  '(:eval
    (propertize
     (hli-format-info-item (if which-func-mode
                               (or (gethash (selected-window) which-func-table)
                                   which-func-unknown)
                             nil))
     'face 'hli-face-which-func)))

(put 'hli-which-func-header-line-format 'risky-local-variable t)

;;; MEMO:
;;; which-func 側で force-mode-line-update が call されるので、
;;; header-line-format 側では何もしなくても同時に header-line も更新されるはず

(defun hli-which-func-enable ()
  'hli-which-func-header-line-format)

(defun hli-which-func-disable ()
  'hli-which-func-header-line-format)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface hli-face-flymake-normal
  '((((background dark))
     (:inherit hli-face-header-line))
    (((background light))
     (:inherit hli-face-header-line)))
  ""
  :group 'header-line-info)

(defface hli-face-flymake-error
  '((((background dark))
     (:inherit hli-face-header-line :foreground "red"))
    (((background light))
     (:inherit hli-face-header-line :foreground "red")))
  ""
  :group 'header-line-info)

(defface hli-face-flymake-warn
  '((((background dark))
     (:inherit hli-face-header-line :foreground "DarkOrange"))
    (((background light))
     (:inherit hli-face-header-line :foreground "DarkOrange")))
  ""
  :group 'header-line-info)

(defface hli-face-flymake-info
  '((((background dark))
     (:inherit hli-face-header-line :foreground "green3"))
    (((background light))
     (:inherit hli-face-header-line :foreground "green3")))
  ""
  :group 'header-line-info)

(defvar hli-information-flymake
  (hli-make-info 'hli-flymake-enable 'hli-flymake-disable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hook を追加して更新する実装方法はコメントアウト
;; (defvar hli-flyamke-header-line-format nil)
;; (make-variable-buffer-local 'hli-flymake-header-line-format)
;; (put 'hli-flymake-header-line-format 'risky-local-variable t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hli-flymake-header-line-format
  '(:eval (hli-flymake-header-line-string)))
(put 'hli-flymake-header-line-format 'risky-local-variable t)

;;; MEMO:
;;; flymake 側でシンタックスチェックが走る度に force-mode-line-update が call
;;; されるので、header-line-format 側では何もしなくても同時に header-line も更
;;; 新されるはず

(defun hli-flymake-ew2str (head err-count warn-count info-count)
  (format "%s%d err, %d warn" head err-count warn-count))

(defun hli-flymake-select-face (ec wc ic)
  (cond ((> ec 0) 'hli-face-flymake-error)
        ((> wc 0) 'hli-face-flymake-warn)
        ((> ic 0) 'hli-face-flymake-info)
        (t 'hli-face-flymake-normal)))

(defun hli-flymake-header-line-string ()
  (cond
   ((and (featurep 'flycheck) flycheck-mode)
    (if (eq flycheck-last-status-change 'finished)
        (let ((inf (flycheck-count-errors flycheck-current-errors)))
          (let ((ec (or (cdr (assq 'error inf)) 0))
                (wc (or (cdr (assq 'warning inf)) 0))
                (ic (or (cdr (assq 'info inf)) 0)))
            (propertize (hli-format-info-item (hli-flymake-ew2str "FC: "
                                                                  ec wc ic))
                        'face (hli-flymake-select-face ec wc ic))))
      (propertize (hli-format-info-item flycheck-last-status-change)
                  'face 'hli-face-flymake-normal)))

   ((and (featurep 'flymake) flymake-mode)
    (let ((ec (flymake-get-err-count flymake-err-info "e"))
          (wc (flymake-get-err-count flymake-err-info "w")))
      (propertize (hli-format-info-item (hli-flymake-ew2str "FM: " ec wc 0))
                  'face (hli-flymake-select-face ec wc 0))))

   (t
    (propertize (hli-format-info-item nil) 'face 'hli-face-flymake-normal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hook を追加して更新する実装方法はコメントアウト

;; (defvar hli-flymake-after-post-syntax-check-hook nil)
;; (defvar hli-flymake-after-flymake-mode-hook nil)

;; (defadvice flymake-post-syntax-check (after hli-flymake-hook activate)
;;   (run-hooks 'hli-flymake-after-post-syntax-check-hook))

;; (defadvice flymake-mode (after hli-flymake-hook activate)
;;   (run-hooks 'hli-flymake-after-flymake-mode-hook))

;; (defun hli-flymake-update-header-line ()
;;   (setq hli-flymake-header-line-format (hli-flymake-header-line-string))
;;   (force-mode-line-update))

;;; MEMO (hli-flymake-update-header-line):
;;; flymake のシンタックスチェックが走った後は、mode-line の更新のために
;;; flymake 側で force-mode-line-update が call され、それに伴い header-line
;;; の更新も行われるので、header-line-info 側で force-mode-line-update を call
;;; する必要はないはず。ただ、flymake-mode が off になったときの header-line
;;; の更新は call する必要があるかどうかは不明。

;; (defun hli-flymake-enable ()
;;   (hli-flymake-update-header-line)
;;   (add-hook 'hli-flymake-after-post-syntax-check-hook
;;             'hli-flymake-update-header-line nil t)
;;   (add-hook 'hli-flymake-after-flymake-mode-hook
;;             'hli-flymake-update-header-line nil t)
;;   'hli-flymake-header-line-format)

;; (defun hli-flymake-disable ()
;;   (remove-hook 'hli-flymake-after-post-syntax-check-hook
;;                'hli-flymake-update-header-line t)
;;   (remove-hook 'hli-flymake-after-flymake-mode-hook
;;                'hli-flymake-update-header-line t)
;;   'hli-flyamke-header-line-format)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hli-flymake-enable ()
  'hli-flymake-header-line-format)

(defun hli-flymake-disable ()
  'hli-flyamke-header-line-format)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vcprompt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom hli-vcprompt-command "vcprompt"
  ""
  :group 'header-line-info
  :type 'string
  )

(defcustom hli-vcprompt-cmd-args '("-f" "%n:%b:%p")
  ""
  :group 'header-line-info
  :type '(repeat string))

(defface hli-face-vcprompt
  '((((background dark))
     (:inherit hli-face-header-line))
    (((background light))
     (:inherit hli-face-header-line)))
  ""
  :group 'header-line-info)

(defvar hli-information-vcprompt
  (hli-make-info 'hli-vcprompt-enable 'hli-vcprompt-disable))

(defvar hli-vcprompt-header-line-format nil)
(make-variable-buffer-local 'hli-vcprompt-header-line-format)
(put 'hli-vcprompt-header-line-format 'risky-local-variable t)

(defvar hli-vcprompt-output nil)
(make-variable-buffer-local 'hli-vcprompt-output)

(defun hli-vcprompt-process (buffer cmd args)
  (let ((bufname (format "hli-vcprompt-proc--%s" (buffer-name buffer)))
        (process-connection-type))
    (condition-case err
        (let ((process (apply 'start-file-process bufname buffer cmd args)))
          (set-process-sentinel process 'hli-vcprompt-sentinel)
          (set-process-filter process 'hli-vcprompt-filter))
      (error
       (message "hli-vcprompt: %s" (error-message-string err))))))

(defun hli-vcprompt-sentinel (process event)
  (let ((buffer (process-buffer process)))
    (delete-process process)
    (when (buffer-name buffer) ;; buffer が削除されていないことを確認
      (with-current-buffer buffer
        (unless (string= event "finished\n")
          (message "hli-vcprompt: error"))
        (setq hli-vcprompt-header-line-format
              (propertize (hli-format-info-item hli-vcprompt-output)
                          'face 'hli-face-vcprompt))
        (force-mode-line-update)))))

(defun hli-vcprompt-filter (process output)
  (let ((buffer (process-buffer process)))
    (when (buffer-name buffer) ;; buffer が削除されていないことを確認
      (with-current-buffer buffer
        (setq hli-vcprompt-output output)))))

(defun hli-vcprompt-run-update-process (buffer)
  (with-current-buffer buffer
    (when buffer-file-name
      (setq hli-vcprompt-output nil)
      (hli-vcprompt-process buffer
                            hli-vcprompt-command hli-vcprompt-cmd-args))))

;;; idle 時に vcprompt を走らせる版
(defun hli-vcprompt-run-update-process-idle (buffer)
  (run-with-idle-timer
   idle-update-delay nil #'hli-vcprompt-run-update-process buffer))

(defun hli-vcprompt-hook-func ()
  (hli-vcprompt-run-update-process (current-buffer)))

(defun hli-vcprompt-enable ()
  (setq hli-vcprompt-header-line-format
        (propertize (hli-format-info-item nil) 'face 'hli-face-vcprompt))
  (hli-vcprompt-run-update-process (current-buffer))
  (add-hook 'after-save-hook 'hli-vcprompt-hook-func nil t)
  'hli-vcprompt-header-line-format)

(defun hli-vcprompt-disable ()
  (remove-hook 'after-save-hook 'hli-vcprompt-hook-func t)
  'hli-vcprompt-header-line-format)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hli-enabled-list nil)
(make-variable-buffer-local 'hli-enabled-list)

(defun hli-enable ()
  (unless hli-enabled-list
    (setq hli-orig-header-line-format header-line-format)
    (setq hli-header-line-format
          (loop for info in hli-information-list
                for sym = (hli-call-func info 'enable)
                for i from 0
                collect
                (list (propertize (if (zerop i) "" hli-delimiter)
                                  'face 'hli-face-header-line)
                      sym)))
    (nconc hli-header-line-format
           (list (propertize (make-string (frame-width) ?\ )
                             'face 'hli-face-header-line)))
    (setq header-line-format hli-header-line-format))
  (setq hli-enabled-list hli-information-list))

(defun hli-disable ()
  (when hli-enabled-list
    (setq header-line-format hli-orig-header-line-format)
    (loop for info in hli-enabled-list
          do
          (hli-call-func info 'disable)))
  (setq hli-enabled-list nil))

(define-minor-mode header-line-info-mode
  "display infomation at header-line"
  :global 'header-line-info
  (if header-line-info-mode (hli-enable) (hli-disable)))

(defun hli-turn-on-if-possible ()
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode hli-enable-modes))
      (header-line-info-mode t)))

(define-globalized-minor-mode global-header-line-info-mode
  header-line-info-mode  hli-turn-on-if-possible
  :group 'header-line-info)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hli-toggle-header-line-info-mode ()
  (interactive)
  (if header-line-info-mode
      (header-line-info-mode -1)
    (header-line-info-mode t)))

(setq-default hli-information-list '(hli-information-vcprompt
                                     hli-information-flymake
                                     hli-information-which-func))

(provide 'header-line-info)
