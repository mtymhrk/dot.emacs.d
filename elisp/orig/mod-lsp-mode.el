;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  mod-lsp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'lsp-mode)

;; hook 関数のリストを server-id 毎に保持するハッシュテーブル
(defvar mod-lsp:after-initialized-hooks (make-hash-table))

;; lsp-client の元の initialized-fn を hook 関数リストの先頭に保存する
(defun mod-lsp:save-orig-initialized-fn (server-id func)
  (let ((hook (gethash server-id mod-lsp:after-initialized-hooks ())))
    (puthash server-id (cons func hook) mod-lsp:after-initialized-hooks)))

;; hook 関数を実行するランナー
(defun mod-lsp:run-after-initialized-hook (workspace)
  (let* ((client (lsp--workspace-client workspace))
         (server-id (lsp--client-server-id client))
         (hook (gethash server-id mod-lsp:after-initialized-hooks ())))
    (dolist (fn hook)
      (when fn
        (condition-case err
            (funcall fn workspace)
          (error
           (message "%s" (error-message-string err))
           nil))))))

;; lsp-client の initialized-fn に hook 関数を実行するランナーを設定する
;; その際、元の initialized-fn を hook 関数リストに保存する
(defun mod-lsp:setup-initialized-fn (server-id)
  (when-let ((client (gethash server-id lsp-clients)))
    (let ((initialize-fn (lsp--client-initialized-fn client)))
      (unless (eq initialize-fn 'mod-lsp:run-after-initialized-hook)
        (mod-lsp:save-orig-initialized-fn server-id initialize-fn)
        (setf (lsp--client-initialized-fn client)
              'mod-lsp:run-after-initialized-hook)))))

(defun mod-lsp:append-if-uniq (lst elem)
  (cond
   ((null lst)
    (cons elem nil))
   ((eq (car lst) elem)
    lst)
   (t
    (cons (car lst) (mod-lsp:append-if-uniq (cdr lst) elem)))))

;; hook 関数をリストに追加する
(defun mod-lsp:add-after-initialized-hook (server-id func)
  (mod-lsp:setup-initialized-fn server-id)
  (let ((hook (gethash server-id mod-lsp:after-initialized-hooks ())))
    (puthash server-id
             (mod-lsp:append-if-uniq hook func)
             mod-lsp:after-initialized-hooks)))

(with-eval-after-load 'lsp-clients
  (dolist (client (hash-table-values lsp-clients))
    (let ((server-id (lsp--client-server-id client)))
      (when (gethash server-id mod-lsp:after-initialized-hooks)
        (mod-lsp:setup-initialized-fn server-id)))))


;;; for RLS (Rust)

(defun mod-lsp:rls-set-settings (settings)
  (lsp--set-configuration `(:rust ,settings)))

(provide 'mod-lsp-mode)
