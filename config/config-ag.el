;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ag.el
;;;    Silver Searcher Ag: very fast grep-like program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin for ag

(eval-after-load 'config-popwin
  '(progn
     ;;; ag の検索結果をポップアップで表示
     (add-popwin-special-display-config '(ag-mode :noselect t :stick t))
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  helm-ag
;;;    helm interface for ag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-ag)

;;; デフォルトで検索ディレクトリを入力するよう変更
(defalias 'orig-helm-ag (symbol-function'helm-ag))
(defalias 'orig-helm-do-ag (symbol-function'helm-do-ag))

(defun helm-ag ()
  (interactive)
  (orig-helm-ag (read-directory-name "Search Directory: ")))

(defun helm-do-ag ()
  (interactive)
  (orig-helm-do-ag (read-directory-name "Search Directory: ")))

;;; project を検索ディレクトリとするコマンドを定義
(defun helm-ag-project ()
  (interactive)
  (orig-helm-ag (ag/project-root default-directory)))

(defun helm-do-ag-project ()
  (interactive)
  (orig-helm-do-ag (ag/project-root default-directory)))

(eval-after-load 'config-helm
  '(progn
     (define-key helm-my-command-keymap (kbd "g") 'helm-do-ag)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-do-ad から ag を起動するコマンド

(eval-after-load 'config-helm
  '(progn
     (defun helm-quit-and-exec-ag ()
       (interactive)
       (lexical-let ((pattern helm-pattern)
                     (dir helm-ag--default-directory))
         (helm-run-after-quit
          (lambda () (ag pattern dir)))))

    (defvar my-helm-do-ag-keymap (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "C-c o")
                                     'helm-quit-and-exec-ag)
                                   map))

    (my-helm-override-keymap 'helm-source-do-ag my-helm-do-ag-keymap)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-ag)
