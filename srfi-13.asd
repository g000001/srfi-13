;;;; srfi-13.asd

(cl:in-package :asdf)

(defsystem :srfi-13
  :serial t
  :depends-on (:mbe :srfi-5 :srfi-8 :srfi-23 :srfi-61)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-13")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-13))))
  (load-system :srfi-13)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-13-internal :srfi-13))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

