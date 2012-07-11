(in-package :ll-css)

;; declaration
(declaim (optimize #+sbcl (sb-c::merge-tail-calls 3) #+sbcl
           (sb-c::insert-debug-catch 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize*
    '(optimize (speed 3) (safety 0) (space 0) (debug 0)
       (compilation-speed 0))))

;; vars and constants
(defvar *css-stream* nil)
(defvar *indent-num-spaces* 4)
(defvar *indent-level* 0)
(defvar *space-after-colon* nil)
(defvar *css-readtable* (copy-readtable nil))
(defconstant colon (intern ":"))
(defconstant open-curly (intern "{"))
(defconstant closing-curly (intern "}"))
(defconstant newline (intern "newline"))
(defconstant equal-sign (intern "="))
(defconstant semicolon (intern ";"))
(defconstant comma (intern ","))
(defconstant dot (intern "."))

;; reader

;;environment
(defvar *global-functions* (make-hash-table))
(defvar *global-vars* (make-hash-table))

;; utils

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;; reader

(defun init-css-readtable ()
  (declare #.*optimize*)
  (let ((*readtable* *css-readtable*))
    (set-syntax-from-char #\# #\a)
    (set-syntax-from-char #\' #\a)
    (set-syntax-from-char #\` #\a)
    (setf (readtable-case *readtable*) :preserve)
    (set-macro-character #\: (lambda (stream char)
                               (declare (ignore stream char))
                               colon))
    (set-macro-character #\{ (lambda (stream char)
                               (declare (ignore stream char))
                               open-curly))
    (set-macro-character #\} (lambda (stream char)
                               (declare (ignore stream char))
                               closing-curly))
    (set-macro-character #\= (lambda (stream char)
                               (declare (ignore stream char))
                               equal-sign))
    (set-macro-character #\; (lambda (stream char)
                               (declare (ignore stream char))
                               semicolon))
    (set-macro-character #\, (lambda (stream char)
                               (declare (ignore stream char))
                               comma))
    (set-macro-character #\. (lambda (stream char)
                               (declare (ignore stream char))
                               dot) t)
    (set-macro-character #\Newline (lambda (stream char)
                                     (declare (ignore stream char))
                                     newline))))

(init-css-readtable)

(defmacro get-next ()
  `(read *css-stream* nil nil))

(defun process-css (path)
  (declare #.*optimize*)
  (let ((*readtable* *css-readtable*) (*package* (find-package :ll-css))  (*indent-level* 0))
    (with-output-to-string (*standard-output*)
      (with-open-file (*css-stream* path)
        (process-or-print (get-next))))))

;; environment

(defmacro add-global-function (symbol function)
  `(setf (gethash ,symbol *global-functions*) ,function))

(defmacro remove-global-function (symbol)
  `(remhash ,symbol *global-functions*))

(defmacro add-global-var (symbol value)
  `(setf (gethash ,symbol *global-vars*) ,value))

(defmacro remove-global-var (symbol)
  `(remhash ,symbol *global-vars*))

;; printer

(defgeneric process-item (item n-item))

(defmethod process-item ((item (eql newline)) n-item)
  (when (eq n-item closing-curly)
    (decf *indent-level*))
  (newline-and-indent)
  nil)

(defmethod process-item ((item (eql open-curly)) n-item)
  (write-char #\{)
  (setq *space-after-colon* t)
  (when (eq n-item newline)
    (incf *indent-level*))
  nil)

(defmethod process-item ((item (eql closing-curly)) n-item)
  (write-char #\})
  (setq *space-after-colon* nil)
  (unless (eq n-item newline)
    (write-char #\Space))
  nil)

(defmethod process-item ((item symbol) n-item)
  (let ((symbol-name (symbol-name item))
        skip-next)
    (cond
      ((eq item semicolon)
       (write-char #\;))
      ((string-starts-with-p "@" symbol-name)
       (if (eq n-item equal-sign)
           (progn
             (setq n-item (get-next))
             (if (eq nil n-item)
                 (error "invalid setq form")
                 (progn
                   (setq skip-next t)
                   (setf (gethash item *global-vars*) n-item))))
           (format t "~a " (gethash item *global-vars*))))
      ((aif (gethash item *global-functions* nil)
            (progn
              (apply it n-item)
              (setq skip-next t)
              t)))
      (t
        (write-string symbol-name)
        (when (and
                (not (eq item dot))
                (not (eq n-item colon))
                (not (eq n-item semicolon))
                (not (eq n-item comma))
                (not (eq n-item closing-curly))
                (if (eq item colon)
                    *space-after-colon*
                    t))
          (write-char #\Space))))
    skip-next))

(defmethod process-item ((item string) n-item)
  (write-char #\")
  (write-string item)
  (write-char #\")
  nil)

(defmethod process-item ((item number) n-item)
  (write item)
  nil)

(defmethod process-item (item n-item)
  (format t "~a" item)
  (unless (or (eq n-item colon) (eq n-item semicolon) (eq n-item comma))
    (write-char #\Space))
  nil)

(defun process-or-print (item)
  (declare #.*optimize*)
  #|(when (or (eq item '|/*|) (eq item '|/**|))
    (loop for form = (get-next)
          while (not (eq form '|*/| )))
    (setq item (get-next)))|#
  (unless item
    (return-from process-or-print))
  (let* ((n-item (get-next))
         (skip-next (process-item item n-item)))
    (if skip-next
        (process-or-print (get-next))
        (process-or-print  n-item))))

(defun newline-and-indent ()
  (declare #.*optimize*)
  (write-char #\Newline)
  (loop repeat (* *indent-level* *indent-num-spaces*)
        do (write-char #\Space))
  nil)

(defun string-starts-with-p (datum str)
  (declare #.*optimize*)
  (alexandria:starts-with-subseq datum str))




