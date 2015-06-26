(defun insert-time ()
  "Insert the current time"
  (interactive)
  (insert
   (format-time-string "%a %b %d %H:%M:%S %Z %Y" (current-time))))

(defun insert-c-info ()
  "Insert Header"
  (interactive)
  (insert "/*") (newline)
  (newline)
  (insert "File Name :") (newline)
  (insert "Author    : Hiroki Motoyama") (newline)
  (insert "Date      : ") (insert-time) (newline)
  (newline) (newline)
  (insert "RCS") (newline)
  (insert " $Id$") (newline)
  (newline)
  (insert "*/") (newline))

(provide 'config-insert-c-info)
