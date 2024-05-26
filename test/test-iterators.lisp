;;;; Test non-range iterators
;;;
;;; This is not nearly enough tests
;;;

(in-package :org.tfeb.star/test)

(define-test ("org.tfeb.star" "org.tfeb.star.iterators")
  :depends-on ("org.tfeb.star.sanity")
  :time-limit 10)

(define-test ("org.tfeb.star.iterators" "in-list")
  (dolist (l '(() (1 2 3)))
    (is equal l (collecting (for ((e (in-list l)))
                              (collect e))))
    (pessimizing-for ((p o) (collecting (for ((e (in-list l)))
                                           (collect e))))
      (is equal (p) (o)))))

(when *test-individually*
  (test "org.tfeb.star.iterators" :report *test-report-class*))
