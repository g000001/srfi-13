(cl:in-package "https://github.com/g000001/srfi-13#internals")


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

(defun-inline modulo (number divisor)
  (mod number divisor))

(defun-inline char->integer (char)
  (cl:char-code char))

(defun-inline bitwise-and  (&rest integers)
  (apply #'logand integers))

#|(defun-inline char=? (a b)
  (cl:char= a b))|#

#|(defun-inline char-ci=? (a b)
  (cl:char-equal a b))|#

(defmacro begin (&body body)
  `(progn ,@body))

(defun CHECK-ARG-to-DECLARE (expr)
  (destructuring-bind (ignore pred var name) expr
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
                     ;(defconstant* ,name (function ,name))
                     )
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (defun ,name (,@(restify args))
                       ,decl
                       ,@body)
                     ;(defconstant* ,name (function ,name))
                     )))))
    (symbol `(progn
               (setf (symbol-function ',name&args) (progn ,@body))
               ;(defconstant* ,name&args (progn ,@body))
               ))))

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

(defun make-string (len &optional (init #\Nul))
  (cl:make-string len :initial-element init))

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

(defun butlastatom (list)
  `(,@(butlast list)
      ,(car (last list))))

;;; FIXME
(defmacro let-optionals* (args (&rest binds) &body body)
  (let ((rest (if (tailp '() binds)
                  '()
                  `(&rest ,(cdr (last binds))) )))
    (cl:loop
       :for (var . val+pred) :in (mapcar (lambda (x)
                                           (if (consp x)
                                               x
                                               (list x) ))
                                         (if rest
                                             (butlastatom binds)
                                             binds) )
       :for k :from 0
       :collect (if (cdr val+pred)
                    `(,var (or (and ,(nth 1 val+pred) ,var) ,(nth 0 val+pred)))
                    `(,var (or (nth ,k ,args) ,(nth 0 val+pred))) )
       :into var+default
       :collect var :into new-binds
       :finally (return
                  `(destructuring-bind (&optional ,@new-binds ,@rest)
                                       ,args
                     (declare (ignorable ,@new-binds))
                     (let* (,@var+default)
                       ,@body ))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (typecase list
      (list (if (tailp () list)
                list
              (cl:let ((last (last list)))
                `(,@(butlast list)
                  ,(car last)
                  cl:&rest
                  ,(cdr last)))))
      (symbol `(cl:&rest ,list)))))

(defmacro define-function (name-args &body body)
  (if (consp name-args)
      (destructuring-bind (name . args)
                          name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (setf (fdefinition ',name-args)
               ,(car body)))))


(defmacro check-arg (pred val caller)
  (let ((gval (gensym)))
    `(let ((,gval ,val))
       (if (not (,pred ,val))
           (error ,gval '(function ,caller))
           ,gval))))
