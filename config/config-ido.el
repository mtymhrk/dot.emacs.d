;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package ido
  :commands ido-mode
  :custom
  (ido-max-window-height 0.75)
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-use-filename-at-point 'guess)
  (ido-confirm-unique-completion t)
  (ido-auto-merge-work-directories-length -1)
  (ido-cannot-complete-command 'ido-next-match)
  :bind
  (:map ido-file-completion-map
   ("C-w" . ido-delete-backward-updir)
   ("C-w" . ido-delete-backward-updir)))

(use-package ido-vertical-mode
  :commands ido-vertical-mode
  :custom
  (ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-completing-read+)

(use-package ido-ubiquitous
  :commands ido-ubiquitous-mode)

(ido-mode 1)
(ido-vertical-mode 1)
(ido-ubiquitous-mode 1)

;;; Memo:
;;;  ido を使って補完候補にない新規ファイルを作成する場合は、C-m やReturn で
;;;  はなく、C-j で入力を確定させる

(provide 'config-ido)
