;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight-symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-symbol)

(custom-set-variables '(highlight-symbol-idle-delay 1.0))

(defvar my-highlight-symbol-modes
  '( actionscript-mode
     apache-mode
     bat-generic-mode
     c++-mode
     c-mode
     csharp-mode
     css-mode
     dos-mode
     emacs-lisp-mode
     html-mode
     ini-generic-mode
     java-mode
     javascript-mode
     js-mode
     lisp-interaction-mode
     scheme-mode
     lua-mode
     latex-mode
     makefile-mode
     makefile-gmake-mode
     markdown-mode
     moccur-edit-mode
     nxml-mode
     nxhtml-mode
     outline-mode
     perl-mode cperl-mode
     php-mode
     python-mode
     rc-generic-mode
     reg-generic-mode
     ruby-mode
     sgml-mode
     sh-mode
     squirrel-mode
     text-mode
     tcl-mode
     visual-basic-mode ))

(defun my-highlight-symbol-mode-maybe ()
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode my-highlight-symbol-modes))
      (highlight-symbol-mode 1)))

(define-globalized-minor-mode global-highlight-symbol-mode
  highlight-symbol-mode my-highlight-symbol-mode-maybe
  :group 'auto-highlight-symbol)

(global-highlight-symbol-mode t)

(provide 'config-highlight-symbol)
