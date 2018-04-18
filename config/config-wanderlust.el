(add-to-list 'load-path (concat user-emacs-directory "elisp/wl"))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl" "Write draft with WL." t)

(add-hook 'wl-draft-mode-hook
  '(lambda ()
     (auto-fill-mode t)
     (set-fill-column 60)))

;; C-x m (compose-mail) で Wanderlust のドラフトモードを起動
(autoload 'wl-user-agent-compose "wl-draft" nil t)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))


