;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-elscreen.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'elscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-line のスクリーン番号 face を、複数スクリーン時の場合変更する


(defface mod-elscreen:mode-line-face-multi
  '((((background dark))
     (:foreground "red"))
    (((background light))
     (:foreground "red")))
  "")

(defvar mod-elscreen:orig-elscreen-mode-line-update
  (symbol-function 'elscreen-mode-line-update))

(defun mod-elscreen:mode-line-update ()
  (when (elscreen-screen-modified-p 'elscreen-mode-line-update)
    (setq elscreen-mode-line-string
          (let ((str (format "[%d]" (elscreen-get-current-screen))))
            (if (> (elscreen-get-number-of-screens) 1)
                (propertize str 'face 'mod-elscreen:mode-line-face-multi)
              str)))
    (force-mode-line-update)))

;;; elscreen の mode-line 更新関数を自前のものに差し替える関数
(defun mod-elscreen:enable-colored-elscreen-mode-line ()
  (interactive)
  (fset 'elscreen-mode-line-update
        (symbol-function 'mod-elscreen:mode-line-update)))

;;; elscreen の mode-line 更新関数を元に戻す関数
(defun mod-elscreen:disable-colored-elscreen-mode-line ()
  (interactive)
  (fset 'elscreen-mode-line-update
        mod-elscreen:orig-elscreen-mode-line-update))

;;; mode-line-format 内のシンボルの risky-loca-variable プロパティが nil
;;; の場合、face 等の設定が無視されるので t にする
(put 'elscreen-mode-line-string 'risky-local-variable t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 新規作成 screen のデフォルトの window 構成を変更する

(defvar mod-elscreen:default-window-config-func nil)


(defvar mod-elscreen:orig-elscreen-default-window-configuration
  (symbol-function 'elscreen-default-window-configuration))


(defun mod-elscreen:default-window-configuration ()
  (let ((default-buffer (get-buffer elscreen-default-buffer-name)))
    (save-window-excursion
      (set-window-dedicated-p (selected-window) nil)
      (delete-other-windows)
      (if default-buffer
          (switch-to-buffer default-buffer)
        (switch-to-buffer (get-buffer-create elscreen-default-buffer-name))
        (funcall elscreen-default-buffer-initial-major-mode)
        (insert elscreen-default-buffer-initial-message)
        (set-buffer-modified-p nil))
      (when mod-elscreen:default-window-config-func
        (funcall mod-elscreen:default-window-config-func))
      (elscreen-current-window-configuration))))

;;; デフォルトの window 構成を作成する関数を自前のものに差し替える関数
(defun mod-elscreen:enable-default-window-configuration-change ()
  (interactive)
  (fset 'elscreen-default-window-configuration
        (symbol-function 'mod-elscreen:default-window-configuration)))

;;; デフォルトの window 構成を作成する関数を元に戻す関数
(defun mod-elscreen:disable-default-window-configuration-change ()
  (interactive)
  (fset 'elscreen-default-window-configuration
        mod-elscreen:orig-elscreen-default-window-configuration))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elsreen-server.el の代替

;;; elscreen-server.el v0.2.0 では emacsclient にファイル名を指定せず、--eval
;;; オブションを指定した場合、エラーになってしまうので、elscreen-server.el の
;;; 使用を止めて、v0.2.0 をベースの代替コードを利用する

(defmacro elscreen-server-defcustom-dont-use-dedicated-frame (type)
  `(defcustom elscreen-server-dont-use-dedicated-frame t
     "*Non-nil to use dframe-attached-frame if frame is dedicated"
     :type 'boolean
     :group ,type))

(defun elscreen-server-visit-files-new-screen (buffer-list)
  (let* ((selected-frame (selected-frame))
         (dframe-attached-frame (and (fboundp 'dframe-attached-frame)
                                     (dframe-attached-frame selected-frame))))
    (when (and elscreen-server-dont-use-dedicated-frame
               (framep dframe-attached-frame))
      (select-frame dframe-attached-frame))
    (elscreen-goto (car (mapcar
                         (lambda (buffer)
                           (elscreen-find-screen-by-buffer buffer 'create))
                         buffer-list)))
    (elscreen-notify-screen-modification 'force-immediately)
    (select-frame selected-frame)))

(with-eval-after-load 'server
  ;; For server.el distributed with GNU Emacs
  (elscreen-server-defcustom-dont-use-dedicated-frame 'server)

  (defun mod-elscreen:server-visit-files (orig-fun &rest args)
    (let ((x (apply orig-fun args)))
      (elscreen-server-visit-files-new-screen
       (cond ((null x)
              ;; emacsclient の引数にファイルが名が指定されていない場合は、
              ;; --eval で指定されて式評価用に screen を 1 つ作成する
              (list (get-buffer server-buffer)))
             ((processp (car x))
              ;; Before multi-tty; server-visit-files returns a list of proc
              ;; and client-record.
              (cdr x))
             ;; After multi-tty was merged; in server.el r1.131 or later, it
             ;; returns only client-record.
             (t
              x)))))
  (advice-add 'server-visit-files :around #'mod-elscreen:server-visit-files))


(provide 'mod-elscreen)
