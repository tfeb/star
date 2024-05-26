;;;; Just check things are basically sane
;;;

(in-package :org.tfeb.star/test)

(define-test ("org.tfeb.star" "org.tfeb.star.sanity"))

(define-test ("org.tfeb.star.sanity" "repeat")
  (is = 3 (length (collecting
                    (for ((i (repeat 3)))
                      (collect i)))))
  (fail (for ((_ (repeat 3 :error t))))))

(define-test ("org.tfeb.star.sanity" "pessimizing")
  (let ((r (random 10)))
    (pessimizing-for ((p o) (collecting
                              (for ((i (repeat r)))
                                (collect i))))
      (is equal (p) (o)))))

(when *test-individually*
  (test "org.tfeb.star.sanity" :report *test-report-class*))
