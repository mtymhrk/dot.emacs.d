;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- lexical-binding: t -*-
;;; mod-counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ivy)
(require 'counsel)


;;; ag を起動するディレクトリを常に問うバージョンの counsel-ag
(defun mod-counsel:counsel-ag ()
  (interactive)
  (if current-prefix-arg
      (call-interactively #'counsel-ag)
    (setq current-prefix-arg 4)
    (counsel-ag nil nil "")))

;;; memo ディレクトリの検索用 counsel
(defun mod-counsel:counsel-grep-my-memo ()
  (interactive)
  (counsel-ag "" "~/memo/"))

(defun mod-counsel:counsel-open-my-memo ()
  (interactive)
  (let ((default-directory "~/memo/org"))
    (counsel-find-file)))

;;; counsel-recentf で表示される候補のファイル名のホーディレクトリ部分を ~ に変
;;; 換するアドバイス

(defun mod-counsel:ad-counsel-recentf-conv-home-dir (orig &rest args)
  (let ((orig-recentf-list recentf-list))
    (unwind-protect
        (progn
          (setq recentf-list (mapcar #'abbreviate-file-name recentf-list))
          (apply orig args))
      (setq recentf-list orig-recentf-list))))

(advice-add 'counsel-recentf :around #'mod-counsel:ad-counsel-recentf-conv-home-dir)

(defvar mod-counsel:take-over-input-text nil)


;;; ivy-read のキーワード引数 :initial-input が指定されておらず、
;;; mod-counsel:take-over-input-text に値が設定されている場合、
;;; mod-counsel:take-over-input-text の値で :initial-input を指定するアドバイス

(defun mod-counsel:ad-ivy-read-add-initial-input (orig &rest args)
  (let ((args (cond
               ((memq :initial-input args)
                args)
               (mod-counsel:take-over-input-text
                (append args `(:initial-input ,mod-counsel:take-over-input-text)))
               (t
                args))))
    (setq mod-counsel:take-over-input-text nil)
    (apply orig args)))

(advice-add 'ivy-read :around #'mod-counsel:ad-ivy-read-add-initial-input)


;;; ivy で補完中に C-. で mod-counsel:next-counsel に設定している counsel を実行
;;; する。

(defvar mod-counsel:next-counsel nil)

(defun mod-counsel:next-counsel (caller)
  (cdr (assq caller mod-counsel:next-counsel)))

(defun mod-counsel:run-next-counsel ()
  (interactive)
  (setq mod-counsel:take-over-input-text ivy-text)
  (let ((next (mod-counsel:next-counsel (ivy-state-caller ivy-last))))
    (when next
      (ivy-quit-and-run
        (funcall next)))))

(define-key ivy-minibuffer-map (kbd "C-.") 'mod-counsel:run-next-counsel)


;;;;;;;;;;;;;;;
(setq mod-counsel:next-counsel
      '((ivy-switch-buffer . counsel-recentf)
        (counsel-recentf . counsel-fzf)
        (counsel-fzf . ivy-switch-buffer)
        (swiper . counsel-imenu)
        (counsel-imenu . swiper)))

(provide 'mod-counsel)
