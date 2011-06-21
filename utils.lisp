(in-package :srfi-13-internal)


;; from KMRCL
(defmacro defconstant* (sym value &optional doc)
  "Ensure VALUE is evaluated only once."
   `(defconstant ,sym (if (boundp ',sym)
                          (symbol-value ',sym)
                          ,value)
     ,@(when doc (list doc))))

(defmacro defun-inline (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args)
       ,@body)))

(defun-inline map (function list &rest more-list)
  (apply #'mapcar function list more-list))

(defun-inline mem (function item list)
  (cl:member item list :test function))

(defun-inline null? (obj) (null obj))

(defun-inline eq? (x y) (eq x y))

(defun-inline pair? (obj) (consp obj))

(defun-inline zero? (x) (zerop x))

(defun-inline set-car! (list obj)
  (rplaca list obj))

(defun-inline set-cdr! (cons x)
  (rplacd cons x))

(defun-inline equal? (x y)
  (equal x y))

(defun-inline memq (x list)
  (cl:member x list :test #'eq))

(defmacro begin (&body body)
  `(progn ,@body))

(defun CHECK-ARG-to-DECLARE (expr)
  (destructuring-bind (ignore var pred name) expr
    (declare (ignore ignore name))
    `(declare ((satisfies ,pred) ,var))))

(defun restify (expr)
  (etypecase expr
    (symbol (list 'cl:&rest expr))
    (list (if (tailp () expr)
              expr
              (let ((last (last expr)))
                (append (butlast expr)
                        (list (car last)
                              'cl:&rest
                              (cdr last))))))))

(defmacro define (name&args &body body)
  (etypecase name&args
    (list (destructuring-bind (name &rest args)
                              name&args
            (destructuring-bind (decl &rest body) body
              (if (cl:string= 'check-arg (car decl))
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (defun ,name (,@(restify args))
                       ,(CHECK-ARG-to-DECLARE decl)
                       ,@body)
                     (defconstant* ,name (function ,name)))
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (defun ,name (,@(restify args))
                       ,decl
                       ,@body)
                     (defconstant* ,name (function ,name)))))))
    (symbol `(progn
               (setf (symbol-function ',name&args) (progn ,@body))
               (defconstant* ,name&args (progn ,@body))))))

(define any #'cl:some)
;(define foldl #'srfi-1:fold)
;(define pair-foldl #'srfi-1:pair-fold)
(define procedure? #'cl:functionp)
(define string? #'cl:stringp)
(defun string-length (str)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string  str))
  (length str))
(define integer? #'cl:integerp)
(define exact? #'cl:rationalp)

(defun substring (str start end)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string  str))
  (subseq str start end))

(defun make-string (len)
  (cl:make-string len))

(defun string-set! (string k char)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string  string))
  (setf (char string k) char))

(defun string-ref (string k)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string string))
  (char string k))

(define char=? #'cl:char=)
(define char<? #'cl:char<)
(define char-ci<? #'cl:char-lessp)
(define char-ci=? #'cl:char-equal)
(define char? #'cl:characterp)


;; a la lisp1
(defconstant* equal? #'cl:equal)
(defconstant* eq? #'cl:eq)
(defconstant* eqv? #'cl:eql)
(defconstant* pair? #'cl:consp)
