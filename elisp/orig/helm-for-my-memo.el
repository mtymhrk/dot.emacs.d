;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-capture と open-junk-file で作成したファイルを、helm を使って grep 検
;;; 索するためのコマンドと再オープンのためのコマンドを定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; このファイルをロードは、config/org-mode.el と config/open-junk-file をロー
;;; ドした後に行う必要がある

(eval-when-compile (require 'cl))
(require 'em-glob)
(require 'helm-files)
(require 'helm-grep)

(defun helm-for-my-memo:path-join (&rest paths)
  (reduce #'(lambda (x y) (concat (file-name-as-directory x) y)) paths))

(defvar helm-for-my-memo:memo-items '())

(defun helm-for-my-memo:completion-read-item ()
  (helm :sources `((name . "Memo Items")
                   (candidates . ,(loop for i in helm-for-my-memo:memo-items
                                        collect (car i)))
                   ;; (candidates . helm-for-my-memo:memo-items)
                   ;; (filter-one-by-one . car)
                   (action . (lambda (x) x)))
        :buffer "helm read memo items"))

(defun helm-grep-my-memo:file (path)
  (helm-do-grep-1 (if (consp path) path (list path))))

(defun helm-grep-my-memo:dir (path glob)
  (helm-do-grep-1 (if (consp path) path (list path)) t nil (list glob)))

(defun helm-grep-my-memo:aux (item)
  (let ((kind (cadr item))
        (path (caddr item))
        (glob (cadddr item)))
    (cond
     ((eq 'file kind)
      (helm-grep-my-memo:file path))
     ((eq 'dir kind)
      (helm-grep-my-memo:dir path glob)))))

(defun helm-grep-my-memo (&optional name)
  (interactive "P")
  (when (null name)
    (setq name (helm-for-my-memo:completion-read-item)))
  (let ((item (loop for i in helm-for-my-memo:memo-items
                    if (equal name (car i)) return i)))
    (let ((org-strtup-folded 'showeverything))
      (helm-grep-my-memo:aux item))))

(defun helm-reopen-my-memo:source (item)
  (let ((kind (cadr item))
        (path (caddr item))
        (glob (cadddr item)))
    (when (not (consp path))
      (setq path (list path)))
    (let ((files (cond
                  ((eq 'file kind)
                   path)
                  ((eq 'dir kind)
                   (loop for p in path append
                         (loop for f in (eshell-extended-glob
                                         (helm-for-my-memo:path-join
                                          p "**" glob))
                               unless (file-directory-p f) collect f))))))
      `((name . ,(format "Reopen memo: %s" (car item)))
        (candidates . ,files)
        (keymap . ,helm-generic-files-map)
        (no-delay-on-input)
        (help-message . helm-generic-file-help-message)
        (mode-line . helm-generic-file-mode-line-string)
        (type . file)))))

(defun helm-reopen-my-memo:aux (item)
  (helm :sources (helm-reopen-my-memo:source item)
        :buffer "*helm for reopen memos"))

(defun helm-reopen-my-memo (&optional name)
  (interactive "P")
  (when (null name)
    (setq name (helm-for-my-memo:completion-read-item)))
  (let ((item (loop for i in helm-for-my-memo:memo-items
                    if (equal name (car i)) return i)))
    (let ((org-startup-folded 'showeverything))
      (helm-reopen-my-memo:aux item))))

(provide 'helm-for-my-memo)