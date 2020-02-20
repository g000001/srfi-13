;;;; srfi-13.asd

(cl:in-package :asdf)


(defsystem :srfi-13
  :version "20200221"
  :description "SRFI 13 for CL: String Libraries"
  :long-description "SRFI 13 for CL: String Libraries
https://srfi.schemers.org/srfi-88"
  :author "Olin Shivers"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:mbe :srfi-5 :srfi-8 :srfi-23 :srfi-61 :srfi-14)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-13")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-13))))
  (let ((name "https://github.com/g000001/srfi-13")
        (nickname :srfi-13))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-13))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-13#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-13)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
