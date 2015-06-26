;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;;; elisp のインストール場所
(setq package-user-dir (concat user-emacs-directory "elpa"))

;;; リポジトリ Marmalade を追加
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;;; リポジトリ MELPA を追加
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ELPA でインストールしたパッケージの再コンパイル用コマンド

(defun my-package-recompile (pkg)
  (unless (package-installed-p pkg)
    (error "failed to recompile a package: uninstalled package: %s" pkg))
  (when (package-built-in-p pkg)
    (error "failed to recompile a package: built-in pacakge: %s" pkg))
  (let ((desc (cadr (assq pkg package-alist))))
    ;; async パッケージが導入されていると (helm をインストールすると自動的に
    ;; インストールされる) デフォルトで helm が非同期にコンパイルされるよう
    ;; になるが、うまく動かないので async byte compile を抑制してコンパイル
    ;; を実行する
    (let ((async-bytecomp-allowed-packages nil))
      (package--compile desc))
    ))

(defun my-package-recompile-all ()
  (dolist (pkg package-alist)
    (let* ((name (package-desc-name (cadr pkg))))
      (unless (package-built-in-p name)
        (my-package-recompile name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'config-package)
