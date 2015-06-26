;;; anything-flymake.el --- list flymake errors using anything

;; Author : this file is from http://d.hatena.ne.jp/kiris60/20091003
;; modified by Yuta Namiki

;; Keywords: flymake anything

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'anything)
(require 'flymake)

(defvar anything-flymake-err-list nil)

(defvar anything-c-source-flymake
  '((name . "Flymake")
    (init . (lambda ()
              (setq anything-flymake-err-list
                    (loop for err-info in flymake-err-info
                          for err = (nth 1 err-info)
                          append err))))
    (candidates . anything-get-flymake-candidates)
    (action
     . (("Goto line" . (lambda (candidate) (goto-line (flymake-ler-line candidate) anything-current-buffer)))))))

(defface anything-flymake-errline
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'anything)

(defface anything-flymake-warnline
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'anything)

(defun anything-get-flymake-candidates ()
  (mapcar
   (lambda (err)
     (let* ((type (flymake-ler-type err))
            (text (flymake-ler-text err))
            (line (flymake-ler-line err)))
       (cons (propertize
              (format "[%s] %s" line text)
              'face (if (equal type "e") 'anything-flymake-errline 'anything-flymake-warnline))
             err)))
   anything-flymake-err-list))

(defun anything-flymake ()
  (interactive)
  (anything-other-buffer 'anything-c-source-flymake "*anything flymake*"))

(provide 'anything-flymake)