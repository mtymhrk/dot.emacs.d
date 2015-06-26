;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elscreen
;;;   http://www.morishima.net/~naoto/software/elscreen/index.php.ja
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'elscreen)
(eval-after-load 'config-wl
  '(require 'elscreen-wl))
;; (require 'elscreen-server)

;;; タブを表示しない
(setq elscreen-display-tab nil)

;; (setq elscreen-tab-width 12)
;; (setq elscreen-tab-display-kill-screen nil)

;;; elscreen を有効化
(elscreen-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-line のスクリーン番号 face を、複数スクリーン時の場合変更する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface elscreen-mode-line-face-multi
  '((((background dark))
     (:foreground "red"))
    (((background light))
     (:foreground "red")))
  "")

(defvar orig-elscreen-mode-line-update
  (symbol-function 'elscreen-mode-line-update))

(defun my-elscreen-mode-line-update ()
  (when (elscreen-screen-modified-p 'elscreen-mode-line-update)
    (setq elscreen-mode-line-string
          (let ((str (format "[%d]" (elscreen-get-current-screen))))
            (if (> (elscreen-get-number-of-screens) 1)
                (propertize str 'face 'elscreen-mode-line-face-multi)
              str)))
    (force-mode-line-update)))

;;; elscreen の mode-line 更新関数を自前のものに差し替える関数
(defun enable-colored-elscreen-mode-line ()
  (interactive)
  (fset 'elscreen-mode-line-update
        (symbol-function 'my-elscreen-mode-line-update)))

;;; elscreen の mode-line 更新関数を元に戻す関数
(defun disable-colored-elscreen-mode-line ()
  (interactive)
  (fset 'elscreen-mode-line-update
        orig-elscreen-mode-line-update))

;;; mode-line-format 内のシンボルの risky-loca-variable プロパティが nil
;;; の場合、face 等の設定が無視されるので t にする
(put 'elscreen-mode-line-string 'risky-local-variable t)

;;; elscreen の mode-line 更新関数を自前のものに差し替える
(enable-colored-elscreen-mode-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 新規作成 screen のデフォルトの window 構成を変更する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar orig-elscreen-default-window-configuration
  (symbol-function 'elscreen-default-window-configuration))

(defun my-elscreen-default-window-configuration ()
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
      (my-default-window-split)
      (elscreen-current-window-configuration))))

;;; デフォルトの window 構成を作成する関数を自前のものに差し替える関数
(defun enable-elscreen-default-window-configuration-change ()
  (interactive)
  (fset 'elscreen-default-window-configuration
        (symbol-function 'my-elscreen-default-window-configuration)))

;;; デフォルトの window 構成を作成する関数を元に戻す関数
(defun disable-elscreen-default-window-configuration-change ()
  (interactive)
  (fset 'elscreen-default-window-configuration
        orig-elscreen-default-window-configuration))

;;; デフォルトの window 構成を変更
(enable-elscreen-default-window-configuration-change)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elsreen-server.el の代替
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(eval-after-load "server"
  ;; For server.el distributed with GNU Emacs
  '(progn
     (elscreen-server-defcustom-dont-use-dedicated-frame 'server)

     (defadvice server-visit-files (after elscreen-server-visit-files activate)
       (elscreen-server-visit-files-new-screen
        (cond ((null ad-return-value)
               ;;; emacsclient の引数にファイルが名が指定されていない場合は、
               ;;; --eval で指定されて式評価用に screen を 1 つ作成する
               (list (get-buffer server-buffer)))
              ((processp (car ad-return-value))
               ;; Before multi-tty; server-visit-files returns a list of proc
               ;; and client-record.
               (cdr ad-return-value))
              ;; After multi-tty was merged; in server.el r1.131 or later, it
              ;; returns only client-record.
              (t
               ad-return-value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-elscreen)
