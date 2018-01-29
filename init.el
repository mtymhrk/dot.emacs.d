;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; emacs -q -lした時に、user-emacs-directoryが変わるように
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 個別設定ファイルのロード

(defvar my-init-config-dir (concat user-emacs-directory "config/"))
(defvar my-init-config-file-list
  '(;; Basics
    "config-basics"

    ;; Package System
    "config-package"

    ;; Appearance
    "config-frame-and-font"
    "config-theme"

    ;; Minor-modes and Utilities
    "config-exec-path-from-shell"
    "config-whitespace"
    "config-smartrep"
    ;; "config-zlc"
    "config-elscreen"
    "config-recentf"
    "config-compile"
    "config-irony"
    "config-flymake"
    "config-flycheck"
    "config-auto-save-buffers"
    "config-dmacro"
    "config-migemo"
    "config-gtags"
    "config-expand-region"
    "config-pos-tip"
    "config-yasnippet"
    "config-open-junk-file"
    "config-ido"
    ;; "config-anything"
    "config-helm"
    ;; "config-auto-complete"
    "config-company"
    "config-smart-tab"
    "config-sequential-command"
    "config-switch-window"
    "config-undo-tree"
    "config-avy"
    "config-popwin"
    "config-projectile"
    "config-quickrun"
    "config-anzu"
    ;; "config-popup-keybindings"
    "config-which-key"
    "config-volatile-highlights"
    "config-highlight-symbol"
    "config-revbufs"
    "config-smartparens"
    "config-yascroll"
    "config-wgrep"
    "config-multiple-cursors"
    "config-bm"
    "config-fill-column-indicator"

    "config-extend-zap-to-char"
    "config-helm-for-my-memo"
    ;; "config-point-stack"
    "config-display-line-info"
    "config-header-line-info"

    ;; Major-modes
    "config-text-mode"
    "config-org-mode"
    "config-c-mode"
    "config-sh-mode"
    "config-scheme-mode"
    "config-ruby-mode"
    "config-emacs-lisp-mode"
    "config-rust-mode"
    "config-gdb"
    "config-view-mode"
    "config-vc-mode"
    "config-info"
    "config-woman"
    "config-wanderlust"
    "config-w3m"
    "config-dired"
    "config-ag"

    ;; Input Method
    "config-ddskk"

    "config-clean-mode-line"
))

(defun my-load-init-config-files (file-list)
  (let ((load-errors '()))
    (dolist (file file-list)
      (condition-case err
          (require (intern file))
        (error
         (push (cons file err) load-errors)
         (message "Error has occurred while loading `%s': %s"
                  file (error-message-string err)))))
    (unless (null load-errors)
      (with-output-to-temp-buffer "*Config File Load Error*"
        (princ "Configuration File Load Error\n\n")
        (dolist (err load-errors)
          (princ (format "  %s: %s\n"
                         (car err) (error-message-string (cdr err)))))))))

(add-to-list 'load-path my-init-config-dir)
(my-load-init-config-files my-init-config-file-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.1 から custom-set-variables が init.el に書き出されるようになってい
;;; て邪魔なので ~/.emacs.d/custom.el に書き出すよう変更する
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs server の起動

(require 'server)
(unless (server-running-p) (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs の挙動がおかしい場合は調査のため有効にする
;; (toggle-debug-on-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
