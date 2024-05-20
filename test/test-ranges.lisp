;;;; Test preamble
;;;

(in-package :org.tfeb.star/test)

(define-test ("org.tfeb.star" "org.tfeb.star.ranges")
  :depends-on ("org.tfeb.star.sanity")
  :time-limit 10)

(define-test ("org.tfeb.star.ranges" "simple")
  (is = 3 (length (collecting
                    (for ((i (repeat 10))
                          (_ (in-range 3)))
                      (collect i)))))
  (is = 3 (length (collecting
                    (for ((i (in-list '(1 2 3 4)))
                          (_ (in-range 3.0)))
                      (collect i))))))

(define-test ("org.tfeb.star.ranges" "sanity-1")
  (for ((e (in-list '(10 10.0f0 10.0d0))))
    (finish (for ((_ (repeat 20))
                  (_ (in-range e)))))
    (let ((l (collecting
               (for ((_ (repeat 20))
                     (f (in-range e)))
                 (collect f)))))
      (is = 10 (length l))
      (true (typep (first l) (type-of e))))))

(define-test ("org.tfeb.star.ranges" "pessmizing-1")
  (for ((e (in-list '(10 10.0f0 10.0d0))))
    (pessimizing-for ((p o) (collecting
                             (for ((_ (repeat 100))
                                   (f (in-range e)))
                               (collect f))))
      (is equal (p) (o)))))

(when *test-individually*
  (test "org.tfeb.star.ranges" :report *test-report-class*))
