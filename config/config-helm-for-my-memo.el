;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-for-my-memo
;;; 索するためのコマンドと再オープンのためのコマンドを定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-for-my-memo)

;; org-mode と open-junk-file の設定の後にロードされる必要がある

(setq helm-for-my-memo:memo-items
  `(("Org - Todo"
     file  ,(helm-for-my-memo:path-join org-directory "todo.org")        f)
    ("Org - Journal"
     dir   ,my-org-capture-journal-dir                             "*.org")
    ("Org - Note"
     dir   ,my-org-capture-note-dir                                "*.org")
    ("Org - Cheatsheet"
     dir   ,my-org-capture-cheatsheet-dir                          "*.org")
    ("Org - Idea"
     file  ,(helm-for-my-memo:path-join org-directory "ideas.org")       f)
    ("Org - Bookmark"
     file  ,(helm-for-my-memo:path-join org-directory "bookmarks.org")   f)
    ("Org - All"
     dir   ,org-directory                                          "*.org")
    ("Junk File"
     dir   ,my-open-junk-file-base-dir                               "*.*")
    ("All"
     dir   ,(list org-directory my-open-junk-file-base-dir)          "*.*")))

(define-key keymap-ctrl-meta-space (kbd "o g") 'helm-grep-my-memo)
(define-key keymap-ctrl-meta-space (kbd "o r") 'helm-reopen-my-memo)

(provide 'config-helm-for-my-memo)
