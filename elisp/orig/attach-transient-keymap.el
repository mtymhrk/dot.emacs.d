;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; attach-transient-keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'which-key)

(defun attach-transientkey:make-keymap (keylist)
  (let ((map (make-sparse-keymap)))
    (dolist (elm keylist map)
      (define-key map (kbd (nth 0 elm)) (nth 2 elm))
      (dolist (k (cdddr elm))
        (define-key map (kbd k) (nth 1 elm))))))

(defun attach-transientkey:make-message (keylist)
  (let ((msg ""))
    (dolist (elm keylist msg)
      (let ((key (nth 0 elm))
            (desc (nth 1 elm)))
        (when (> (length desc) 0)
          (when (> (length msg) 0)
            (setq msg (concat msg ", ")))
          (setq msg (format "%s[%s] - %s" msg key desc)))))))

(defun attach-transientkey:set-keymap (keylist)
  (set-transient-map (attach-transientkey:make-keymap keylist) t)
  (unless (or (minibufferp)
              (and (featurep 'which-key)
                   which-key-show-transient-maps))
    (message (attach-transientkey:make-message keylist))))

(defmacro attach-transientkey:attach (func keylist)
  (let ((adv-func-sym (intern (concat "attach-transientkey:adv-"
                                      (symbol-name func)))))
    `(progn
       (defun ,adv-func-sym (&rest args)
         (when (called-interactively-p)
           (attach-transientkey:set-keymap ,keylist)))
       (advice-add ',func :after #',adv-func-sym))))

(defmacro attach-transientkey:define-keylist (keymap-name keylist)
  `(progn
     (defvar ,keymap-name ',keylist)
     ,@(mapcar
        #'(lambda (elm)
            (let ((func (nth 2 elm)))
              `(attach-transientkey:attach ,func ,keymap-name)))
        keylist)))

;;;
;;; foobarbaz に transient-keymap を設定する例.
;;; transient-keymap には some-command1 と some-command2 がそれぞれキー「a」、
;;; 「b」にバインドされる
;;;
;;;   (defun foobarbaz ()
;;;     (interactive)
;;;     ...)
;;;
;;;   (defvar temp-key-list '(("a" "AAA" some-command1)
;;;                           ("b" "BBB" some-command2)))
;;;
;;;
;;;   (attach-transientkey:attach foobarbaz temp-key-list)
;;;
;;; 上記三つを一度に行うのが attach-transientkey:define-keylist
;;;
;;;   (attach-transientkey:define-keylist
;;;      temp-key-list
;;;      (("a" "AAA" some-command1)
;;;       ("b" "BBB' some-command2)))
;;;


(provide 'attach-transient-keymap)
