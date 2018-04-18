;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
;;;   http://orgmode.org/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package org-install
  :config
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-directory "~/memo/org/")
  (setq org-default-notes-file (concat org-directory "agenda.org"))

  ;; org-mode で書き換えられたくないキーバインドの書き換えを回避
  (defun my-hook-func-org-mode ()
    (define-key org-mode-map (kbd "C-<tab>") nil)
    (define-key org-mode-map (kbd "C-a") nil)
    (define-key org-mode-map (kbd "C-e") nil)
    (define-key org-mode-map (kbd "C-'") nil))

  ;; ファイルを開いたとき見出しを折り畳まない
  (setq org-startup-folded 'nofold)
  :hook
  ((org-mode . my-hook-func-org-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-capture

(use-package org-capture
  :config
  ;; メモ用のファイル名を作成する関数
  (setq my-org-capture-journal-dir (concat org-directory "journal/"))
  (setq my-org-capture-note-dir (concat org-directory "note/"))
  (setq my-org-capture-cheatsheet-dir (concat org-directory "cheatsheet/"))

  (defun create-journal-file-name ()
    (unless (file-exists-p my-org-capture-journal-dir)
      (make-directory my-org-capture-journal-dir))
    (concat my-org-capture-journal-dir
            (format-time-string "%Y.%m.%d-%H.%M.%S.org")))

  (defun create-note-file-name ()
    (unless (file-exists-p my-org-capture-note-dir)
      (make-directory my-org-capture-note-dir))
    (concat my-org-capture-note-dir
            (format-time-string "%Y.%m.%d-%H.%M.%S.org")))

  (defun create-cheatsheet-file-name ()
    (unless (file-exists-p my-org-capture-cheatsheet-dir)
      (make-directory my-org-capture-cheatsheet-dir))
    (concat my-org-capture-cheatsheet-dir
            (format-time-string "%Y.%m.%d-%H.%M.%S.org")))

  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "todo.org" "Inbox")
           "** TODO %?\n   %i\n   %a\n   %t")
          ("j" "Journal" entry
           (file create-journal-file-name)
           "* %?\n[%T]\n")
          ("n" "Note" entry
           (file create-note-file-name)
           "* %?\n[%T]\n")
          ("c" "Cheatsheet" entry
           (file create-cheatsheet-file-name)
           "* %?\n[%T]\n")
          ("i" "Idea" entry
           (file+headline "ideas.org" "New Ideas")
           "** %?\n   %i\n   %a\n   %t")
          ("b" "Bookmark" entry
           (file+headline "bookmarks.org" "Bookmarks")
           "** %?\n   %i\n   %a\n   %t")))

;;;  %[pathname] insert the contents of the file given by `pathname'.
;;;  %(sexp)     evaluate elisp `(sexp)' and replace with the result.
;;;  %<...>      the result of format-time-string on the ... format specification.
;;;  %t          time stamp, date only.
;;;  %T          time stamp with date and time.
;;;  %u, %U      like the above, but inactive time stamps.
;;;  %a          annotation, normally the link created with `org-store-link'.
;;;  %i          initial content, copied from the active region.  If %i is
;;;              indented, the entire inserted text will be indented as well.
;;;  %A          like %a, but prompt for the description part.
;;;  %c          current kill ring head.
;;;  %x          content of the X clipboard.
;;;  %k          title of currently clocked task.
;;;  %K          link to currently clocked task.
;;;  %n          user name (taken from `user-full-name').
;;;  %f          file visited by current buffer when org-capture was called.
;;;  %F          full path of the file or directory visited by current buffer.
;;;  %:keyword   specific information for certain link types, see below.
;;;  %^g         prompt for tags, with completion on tags in target file.
;;;  %^G         prompt for tags, with completion on all tags in all agenda files.
;;;  %^t         like %t, but prompt for date.  Similarly %^T, %^u, %^U.
;;;              You may define a prompt like %^{Please specify birthday.
;;;  %^C         interactive selection of which kill or clip to use.
;;;  %^L         like %^C, but insert as link.
;;;  %^{prop}p   prompt the user for a value for property `prop'.
;;;  %^{prompt}  prompt the user for a string and replace this sequence with it.
;;;              A default value and a completion table ca be specified like this:
;;;              %^{prompt|default|completion2|completion3|...}.
;;;  %?          After completing the template, position cursor here.

  ;; org-agenda
  (setq org-agenda-files
        (list org-directory my-org-capture-journal-dir my-org-capture-note-dir))

  ;; todo
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  :bind
  ("C-c o c" . org-capture)
  ("C-c o a" . org-agenda)
  (:map keymap-ctrl-meta-space
        ("o c" . org-capture)
        ("o a" . org-agenda)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-org-mode)
