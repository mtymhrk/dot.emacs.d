;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight-column-limit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface hcl:highlight-face
  '((((class color) (background dark))
     (:background "gray30"))
    (((class color) (background light))
     (:background "gray90")))
  "Face used to mark number of characters limit"
  :group 'faces)

(defun hcl:build-regexp (limit)
  (format "^[^\n]\\{%d\\}\\(.\\)" limit))

(defun hcl:highlight-column-limit (limit)
  (font-lock-add-keywords nil
                          `((,(hcl:build-regexp limit)
                             1 'hcl:highlight-face t))))


(defmacro hcl:add-hook (hook limit)
  (let ((hook-func-name (intern (format "hcl:%s--highlight-col-limit" hook))))
    `(progn
       (defun ,hook-func-name ()
         (hcl:highlight-column-limit ,limit))
       (add-hook ',hook ',hook-func-name))))

(provide 'highlight-column-limit)
