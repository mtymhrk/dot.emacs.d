;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :commands ivy-mode
  :delight
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

(use-package counsel
  :config

  ;; ag を起動するディレクトリを常に問うバージョンの counsel-ag
  (defun my-counsel-ag ()
    (interactive)
    (if current-prefix-arg
        (call-interactively #'counsel-ag)
      (setq current-prefix-arg 4)
      (counsel-ag nil nil "")))

  :bind
  ("C-x C-f" . counsel-find-file)
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history))
  (:map counsel-find-file-map
        ("C-w" . counsel-up-directory)))
