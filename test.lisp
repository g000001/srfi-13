(cl:in-package "https://github.com/g000001/srfi-13#internals")


(def-suite* srfi-13)


(defmacro iss (x y)
  `(is (string= ,x ,y)))


(defmacro isqu (x y)
  `(is (equal ,x ,y)))


(define (proc x . args)
  (let-optionals* args ((a 'a)
                        (b 'b)
                        (c 'c))
    (list x a b c)))


(define (proc2 . args)
  (let-optionals* args ((a 'a) . b)
    (list a b)))


(define (proc3 . args)
  (let-optionals* args ((a 0)
                        (b (+ a 1))
                        (c (+ b 1)))
    (list a b c)))


(test let-optionals*
  (isqu (proc 0) '(0 A B C))
  (isqu (proc 0 1) '(0 1 B C))
  (isqu (proc 0 1 2) '(0 1 2 c))
  (isqu (proc 0 1 2 3) '(0 1 2 3))
  (isqu (proc2) '(a ()))
  (isqu (proc2 0) '(0 ()))
  (isqu (proc2 0 1) '(0 (1)))
  (isqu (proc2 0 1 2) '(0 (1 2)))
  (isqu (proc3) '(0 1 2))
  (isqu (proc3 8) '(8 9 10))
  (isqu (proc3 8 2) '(8 2 3))
  (isqu (proc3 8 2 -1) '(8 2 -1)))


(test |Predicates|
  ;; string?
  (is-true (string? "aaa"))
  (is-false (string? nil))
  ;; string-null?
  (is-true (string-null? ""))
  (is-false (string-null? "8"))
  ;; string-every
  (is-true (string-every #\a "aaaa"))
  (is-true (string-every #\a "aaaab" 0 4))
  (is-true (string-every (srfi-14:string->char-set "ab") "aaaa"))
  (is-true (string-every (srfi-14:string->char-set "ab") "aaaa" 0))
  (is-true (string-every (lambda (x) (char=? #\a x)) "aaaa"))
  (is-true (string-every (srfi-14:string->char-set "ab") "aaab"))
  ;; string-any
  (is-true (string-any #\a "bbbba"))
  (is-true (string-any #\a "123ab" 0 4))
  (is-true (string-any (srfi-14:string->char-set "ab") "aaaa"))
  (is-true (string-any (srfi-14:string->char-set "ab") "aaaa" 0))
  (is-true (string-any (lambda (x) (char=? #\a x)) "aaaa"))
  (is-true (string-any (srfi-14:string->char-set "ab") "aaab")))


(test |Constructors|
  (is (string= (make-string 4 #\.)
               "...."))
  (is (string= (string #\a #\b #\c #\d)
               "abcd"))
  (is (string= (string-tabulate (lambda (x) (code-char (+ 65 x))) 26)
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))


(test |List & string conversion|
  (is (equal (string->list "ABCD")
             '(#\A #\B #\C #\D)))
  (is (equal (string->list "ABCD" 0)
             '(#\A #\B #\C #\D)))
  (is (equal (string->list "ABCD" 1)
             '(#\B #\C #\D)))
  (is (equal (string->list "ABCD" 1 2)
             '(#\B)))
  (is (string= (list->string '(#\A #\B #\C #\D))
               "ABCD"))
  (is (string= (reverse-list->string '(#\A #\B #\C #\D))
               "DCBA"))
  ;; string-join
  (is (string= (string-join '("foo" "bar" "baz") ":")
             "foo:bar:baz"))
  (is (string= (string-join '("foo" "bar" "baz") ":" :suffix)
               "foo:bar:baz:"))
  (is (string= (string-join '()   ":")
               ""))
  (is (string= (string-join '("") ":")
               "")))


(test |Selection|
  (is (= 4 (string-length "abcd")))
  (is (char= #\b (string-ref "abcd" 1)))
  (is (string= (string-copy "1234" 0 4)
               "1234"))
  (is (string= (string-copy "1234" 0 3)
               "123"))
  (is (string= (string-copy "1234" 1)
               "234"))
  (iss (string-copy "Beta substitution")
       "Beta substitution")
  (iss (string-copy "Beta substitution" 1 10)
       "eta subst")
  (iss (string-copy "Beta substitution" 5)
       "substitution")
  (signals (cl:error)
    (string-copy! (string-copy "Microsoft") 0
                  "Regional Microsoft Operating Companies"))
  (iss (string-take "Pete Szilagyi" 6)
       "Pete S")
  (iss (string-drop "Pete Szilagyi" 6)
       "zilagyi")
  (iss (string-take-right "Beta rules" 5)
       "rules")
  (iss (string-drop-right "Beta rules" 5)
       "Beta ")
  (signals (cl:error)
    (string-take "foo" 37))
  (iss (string-pad     "325" 5)
       "  325")
  (iss (string-pad   "71325" 5)
       "71325")
  (iss (string-pad "8871325" 5)
       "71325")
  (iss (string-trim-both "  The outlook wasn't brilliant,
")
       "The outlook wasn't brilliant,"))


(define-function (f) (make-string 3 #\*))


(define-function (g) "***")


(test |Modification|
  (string-set! (f) 0 #\?)
  #|(signals (cl:error)
    (string-set! (g) 0 #\?))|#
  #|(signals (cl:error)
    (string-set! (cl:string 'immutable)
                 3
                 #\?))|#
  (iss (let ((s (make-string 4)))
         (string-fill! s #\a)
         s)
       "aaaa"))


(test |Comparison|
  (= 5 (string-compare "The cat in the hat" "abcdefgh"
                       #'values #'values #'values
                       4 6                  ; Select "ca"
                       2 4))                ; & "cd"
  (is-true (<= 0 (string-hash "abcdefg" 8) (- 8 1))) ; When B > 0.
  (is-true (and (string=    "s1" "s1")
                (= (string-hash "s1" 3)
                   (string-hash "s1" 3))))
  (is-true (and (string-ci=     "s1" "S1")
                (= (string-hash-ci "s1" 3)
                   (string-hash-ci "S1" 3)))))


(test |Searching|
  (is (= (string-contains "eek -- what a geek." "ee"
                          12 18) ; Searches "a geek"
         15)))


(test |Alphabetic case mapping|
  (iss (string-titlecase "--capitalize tHIS sentence.")
       "--Capitalize This Sentence.")
  (iss (string-titlecase "see Spot run. see Nix run.")
       "See Spot Run. See Nix Run.")
  (iss (string-titlecase "3com makes routers.")
       "3Com Makes Routers.")
  (iss (string-titlecase "greasy fried chicken" 2)
       "Easy Fried Chicken"))


(test |Reverse & append|
  (iss (string-reverse "Able was I ere I saw elba.")
       ".able was I ere I saw elbA")
  (iss (let ((s "123456789"))
         (let ((i (modulo 4 (string-length s))))
           (string-reverse! s 0 i)
           (string-reverse! s i)
           (string-reverse! s))
         s)
       (copy-seq "567891234"))
  (iss (string-concatenate-reverse '(" must be" "Hello, I") " going.XXXX" 7)
       "Hello, I must be going."))


(defun char-lower-case? (char)
  (and (char/= (char-downcase char)
               (char-upcase char))
       (char= (char-downcase char)
              char)))


(test |Fold, unfold & map|
  (isqu (string-fold-right #'cons '() "foo")
        '(#\f #\o #\o) )
  ;; Count the number of lower-case characters in a string.
  (is (= 3 (string-fold (lambda (c count)
                          (if (char-lower-case? c)
                              (+ count 1)
                              count ))
                        0
                        "FOO bar BAZ" )))
  ;; Double every backslash character in S.
  (iss (let ((s "\\foo\\ bar baz\\"))
         (let* ((ans-len (string-fold (lambda (c sum)
                                        (+ sum (if (char=? c #\\) 2 1)))
                                      0 s))
                (ans (make-string ans-len)) )
           (string-fold (lambda (c i)
                          (let ((i (if (char=? c #\\)
                                       (begin (string-set! ans i #\\) (+ i 1))
                                       i )))
                            (string-set! ans i c)
                            (+ i 1) ))
                        0 s)
           ans ))
       "\\\\foo\\\\ bar baz\\\\" ))


(test |Replicate & rotate|
  (iss (xsubstring "abcdef" 2)
       "cdefab")
  (iss (xsubstring "abcdef" -2)
       "efabcd")
  (iss (xsubstring "abc" 0 7)
       "abcabca"))


(define-function (string-insert s i \t) (string-replace s \t i i))


(test |Miscellaneous: insertion, parsing|
  (iss (string-replace "The TCL programmer endured daily ridicule."
                       "another miserable perl drone" 4 7 8 22 )
       "The miserable perl programmer endured daily ridicule." )

  (iss (string-replace "It's easy to code it up in Scheme." "lots of fun" 5 9)
       "It's lots of fun to code it up in Scheme." )
  (iss (string-insert "It's easy to code it up in Scheme." 5 "really ")
       "It's really easy to code it up in Scheme." )
  (isqu (string-tokenize "Help make programs run, run, RUN!")
        '("Help" "make" "programs" "run," "run," "RUN!")))


(defun eof-object? (obj)
  (eq :eof obj))


;; Read chars from IPORT until we find string PAT or hit EOF.
(define (port-skip pat iport)
  (let* ((rv (make-kmp-restart-vector pat))
         (patlen (string-length pat)))
    (let lp ((i 0) (nchars 0))
      (if (= i patlen) nchars                    ; Win -- nchars skipped
          (let ((c (read-char iport)))
            (if (eof-object? c) c                ; Fail -- EOF
                (lp (kmp-step pat rv c i #'char=? 0) ; Continue
                    (+ nchars 1))))))))


(test |Knuth-Morris-Pratt searching|
  (iss (with-input-from-string (in "foo bar baz")
         (file-position in
                        (port-skip "bar" in) )
         (read-line in) )
       " baz"))


;;; *EOF*
