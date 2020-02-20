;;;; package.lisp

(cl:in-package cl-user)

(defpackage "https://github.com/g000001/srfi-13"
  (:use)
  (:export
   string-map string-map!
   string-fold       string-unfold
   string-fold-right string-unfold-right
   string-tabulate string-for-each string-for-each-index
   string-every string-any
   string-hash string-hash-ci
   string-compare string-compare-ci
   string=    string<    string>    string<=    string>=    string<>
   string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<>
   string-downcase  string-upcase  string-titlecase
   string-downcase! string-upcase! string-titlecase!
   string-take string-take-right
   string-drop string-drop-right
   string-pad string-pad-right
   string-trim string-trim-right string-trim-both
   string-filter string-delete
   string-index string-index-right
   string-skip  string-skip-right
   string-count
   string-prefix-length string-prefix-length-ci
   string-suffix-length string-suffix-length-ci
   string-prefix? string-prefix-ci?
   string-suffix? string-suffix-ci?
   string-contains string-contains-ci
   string-copy! substring/shared
   string-reverse string-reverse! reverse-list->string
   string-concatenate string-concatenate/shared string-concatenate-reverse
   string-append/shared
   xsubstring string-xcopy!
   string-null?
   string-join
   string-tokenize
   string-replace
   ;; R5RS extended
   string->list string-copy string-fill!
   ;; R5RS re-exports
   string? make-string string-length string-ref string-set!
   ;; R5RS re-exports (also defined here but commented-out)
   string string-append list->string))

(defpackage "https://github.com/g000001/srfi-13#internals"
  (:use cl
        fiveam
        mbe
        "https://github.com/g000001/srfi-13"
        "https://github.com/g000001/srfi-8"
        "https://github.com/g000001/srfi-14"
        "https://github.com/g000001/srfi-61")
  (:shadowing-import-from :cl-user :else)
  (:shadowing-import-from "https://github.com/g000001/srfi-61"
                          cond)
  (:shadowing-import-from "https://github.com/g000001/srfi-5"
                          let)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-13" . #.(let (ans)
                                              (do-external-symbols (s "https://github.com/g000001/srfi-13")
                                                (push (string s) ans))
                                         ans))
  (:shadowing-import-from "https://github.com/g000001/srfi-23"
                          error)
  (:shadow map))
