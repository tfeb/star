;;;; Test declarations for Štar 2 and up
;;;
;;; This is pretty much a hack
;;;

(in-package :org.tfeb.star/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or LispWorks SBCL) (error "not implemented"))

(define-test ("org.tfeb.star" "org.tfeb.star.declarations"))

(defmacro varinfo (&rest variables &environment e)
  `(final*
    '(,@(mapcar (lambda (v)
                  `(,v ,@(multiple-value-list
                          #+LispWorks (hcl:variable-information v e)
                          #+SBCL (sb-cltl2:variable-information v e))))
                variables))))

(define-test ("org.tfeb.star.declarations" "for")
  (let ((ldecls (for ((a (in-list '(1 2 3 4 5)))
                      (b (in-list '(1 2 3 5 6))))
                  (declare (ignorable a b)
                           (fixnum a b))
                  (varinfo a b)))
        (sdecls (for ((a (in-list '(1 2 3 4 5)))
                      (b (in-list '(1 2 3 5 6))))
                  (declare (special a b))
                  (varinfo a b))))
    (dolist (ldecl ldecls)
      (destructuring-match ldecl
        ((_ kind _ decls)
         (is eql kind ':lexical)
         (is eql (cdr (assoc 'type decls)) 'fixnum))
        (otherwise
         (error "hopeless: ~S" ldecl))))
    (dolist (sdecl sdecls)
      (destructuring-match sdecl
        ((_ kind _ _)
         (is eql kind ':special))
        (otherwise
         (error "hopeless: ~S" sdecl))))))

(define-test ("org.tfeb.star.declarations" "for*")
  (let ((ldecls (for* ((a (in-list '(1 2 3 4 5)))
                       (b (in-list '(1 2 3 5 6))))
                  (declare (ignorable a b)
                           (fixnum a b))
                  (varinfo a b)))
        (sdecls (for* ((a (in-list '(1 2 3 4 5)))
                      (b (in-list '(1 2 3 5 6))))
                  (declare (special a b))
                  (varinfo a b))))
    (dolist (ldecl ldecls)
      (destructuring-match ldecl
        ((_ kind _ decls)
         (is eql kind ':lexical)
         (is eql (cdr (assoc 'type decls)) 'fixnum))
        (otherwise
         (error "hopeless: ~S" ldecl))))
    (dolist (sdecl sdecls)
      (destructuring-match sdecl
        ((_ kind _ _)
         (is eql kind ':special))
        (otherwise
         (error "hopeless: ~S" sdecl))))))

(when *test-individually*
  (test "org.tfeb.star.declarations" :report *test-report-class*))
