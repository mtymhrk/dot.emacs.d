;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-ubiquitous)

(custom-set-variables '(ido-max-window-height 0.75)
                      '(ido-enable-flex-matching t)
                      '(ido-everywhere t)
                      '(ido-use-filename-at-point 'guess)
                      '(ido-confirm-unique-completion t)
                      '(ido-auto-merge-work-directories-length -1)
                      '(ido-cannot-complete-command 'ido-next-match))

(custom-set-variables '(ido-vertical-define-keys 'C-n-and-C-p-only))

(ido-mode 1)
(ido-vertical-mode 1)
(ido-ubiquitous-mode 1)

(define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
(define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)

;;; Memo:
;;;  ido を使って補完候補にない新規ファイルを作成する場合は、C-m やReturn で
;;;  はなく、C-j で入力を確定させる

(provide 'config-ido)
