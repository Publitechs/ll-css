(in-package :ll-css)

(add-global-function |url| (lambda (path)
                             (declare #'*optimize*)
                             (write-string (format nil "url(~a) " path))))
