;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequential-command
;;;   同じコマンドを連続実行することで挙動を変える
;;;   http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/sequential-command.el
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/sequential-command-config.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package sequential-command-config
  :config
  (defun my-seq-aux-upcase-backword-word ()
    (interactive)
    (upcase-word (- 1)))

  (defun my-seq-aux-capitalize-backword-word ()
    (interactive)
    (capitalize-word (- 1)))

  (defun my-seq-aux-downcase-backword-word ()
    (interactive)
    (downcase-word (- 1)))

  ;; 直前の word を upper case -> capitalize -> lower case に変換する
  (define-sequential-command seq-upcase-capitalize-downcase-backword-word
    my-seq-aux-upcase-backword-word
    my-seq-aux-capitalize-backword-word
    my-seq-aux-downcase-backword-word)

  ;; sequential-command-config に含まれる seq-home に back-to-indentiation
  ;; を加えたバージョンを定義
  (define-sequential-command seq-home2
    back-to-indentation beginning-of-line beginning-of-buffer seq-return)

  :bind
  ("C-a" . seq-home)
  ("M-m" . seq-home2)
  ("C-e" . seq-end)
  ("M-U" . seq-upcase-capitalize-downcase-backword-word))


(provide 'config-sequential-command)
