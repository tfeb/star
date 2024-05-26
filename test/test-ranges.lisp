;;;; Test ranges
;;;
;;; This is not enough tests
;;;

(in-package :org.tfeb.star/test)

(define-test ("org.tfeb.star" "org.tfeb.star.ranges")
  :depends-on ("org.tfeb.star.sanity" "org.tfeb.star.iterators")
  :time-limit 60)                       ;in case it runs away

(define-test ("org.tfeb.star.ranges" "range-sanity")
  ;; Check things actually iterate the way they should
  (is = 3 (length (collecting
                    (for ((_ (repeat 10))
                          (i (in-range 3)))
                      (collect i)))))
  (is = 3 (length (collecting
                    (for ((_ (repeat 10))
                          (i (in-range :from 0 :before 3)))
                      (collect i)))))
  (is = 3 (length (collecting
                    (for ((_ (repeat 10))
                          (i (in-range :from 0 :before -3)))
                      (collect i)))))
  (is = 3 (length (collecting
                    (for ((_ (repeat 4))
                          (i (in-range 3.0)))
                      (collect i)))))
  (is = 3 (length (collecting
                    (for ((_ (repeat 4))
                          (i (in-range -3.0)))
                      (collect i)))))
  (is = 3 (length (collecting
                    (for ((_ (repeat 4))
                          (i (in-range :from 0 :before 3.0)))
                      (collect i)))))
  (is = 3 (length (collecting
                    (for ((_ (repeat 4))
                          (i (in-range :from 0 :before -3.0)))
                      (collect i)))))
  ;; Check some types
  (for ((e (in-list '(10 10.0f0 10.0d0 -10 -10.0f0 -10.0d0))))
    (finish (for ((_ (repeat 20 :error t))
                  (_ (in-range e)))))
    (let ((l (collecting
               (for ((_ (repeat 20 :error t))
                     (f (in-range e)))
                 (collect f)))))
      (is = 10 (length l))
      (true (typep (first l) (type-of e))))
    (let ((l (collecting
               (for ((_ (repeat 20 :error t))
                     (f (in-range :from 0 :before e)))
                 (collect f)))))
      (is = 10 (length l))
      (true (typep (first l) (type-of e))))))

(define-test ("org.tfeb.star.ranges" "range-optimizers-succeeding")
  :depends-on ("range-sanity")
  ;; This knows when the optimizer should succeed and fail.
  (let ((iro (find-iterator-optimizer 'in-range)))
    (true iro)
    (when iro
      ;; Literals, bounded
      (for ((form (in-list
                   (collecting
                     (for ((simple (in-list '((10) (10.0f0) (10.0d0)))))
                       (collect simple))
                     (for* ((s (in-list '(:from :after)))
                            (e (in-list '(:before :to)))
                            (by (in-list '(() (:by 1))))
                            (type (in-list '(() (:type 'fixnum) (:type type)))))
                       (collect `(in-range ,s 0 ,e 10 ,@by ,@type)))))))
        (true (funcall iro form nil)))
      ;; Literals, unbounded
      (for ((form (in-list
                   (collecting
                     (for* ((s (in-list '(:from :after)))
                            (by (in-list '(() (:by 1))))
                            (type (in-list '(() (:type 'fixnum) (:type type)))))
                       (collect `(in-range ,s 0 ,@by ,@type)))))))
        (true (funcall iro form nil)))
      ;; Bounded non-literals. Type is implicitly literal REAL or
      ;; literal
      (for ((form (in-list
                   (collecting
                     (collect '(n))
                     (for ((s (in-list '(:from :after)))
                           (e (in-list '(:before :to)))
                           (by (in-list '(() (:by s))))
                           (type (in-list '(() (:type 'fixnum)))))
                       (collect `(in-range ,s s ,e e ,@by ,@type)))))))
        (true (funcall iro form nil)))
      ;; Unbounded non-literals. Type is implicitly literal REAL or
      ;; literal
      (for ((form (in-list
                   (collecting
                     (collect '(n))
                     (for ((s (in-list '(:from :after)))
                           (by (in-list '(() (:by s))))
                           (type (in-list '(() (:type 'fixnum)))))
                       (collect `(in-range ,s s ,@by ,@type)))))))
        (true (funcall iro form nil)))
      ;; Type is not a literal, this will fail
      (false (funcall iro '(in-range :from a :to b :type type) nil)))))

(define-test ("org.tfeb.star.ranges" "legal-ranges")
  (for ((good-range (in-list '((10)
                               (:from 0 :before 10)
                               (:from 0.0 :before 10)
                               (:from 0 :before 10 :by 1.0)))))
    (finish (apply #'in-range good-range)))
  (for ((bad-range (in-list '((:from 0 :before 10 :type 'float)
                              (:from 0.0 :before 10 :type 'fixnum)))))
    (fail (apply #'in-range bad-range)))
  ;; Testing types of literal ranges is hard
  (let ((u 10.0))
    (finish (for ((_ (in-range :from 0 :before u :type 'float)))))
    (fail (for ((_ (in-range :from 0 :before u :type 'fixnum))))))
  (let ((u 10))
    (finish (for ((_ (in-range :from 0 :before u :type 'fixnum)))))
    (fail (for ((_ (in-range :from 0 :before u :type 'float)))))))

(define-test ("org.tfeb.star.ranges" "pessimized-range-optimizers")
  :depends-on ("range-sanity")
  (macrolet ((tce (&rest range)
               `(pessimizing-for ((p o) (collecting
                                          (for ((_ (repeat 100 :error t))
                                                (e (in-range ,@range)))
                                            (collect e))))
                  (is equal (p) (o)))))
    (tce 10)
    (tce :from 0 :before 10)
    (tce :from 0.0 :before 10)
    (tce :from 0 :before 10 :by 1.0)
    (tce :from 0 :before 10 :type 'fixnum)
    (tce :from 0 :before 10.0f0 :type 'single-float)))

(when *test-individually*
  (test "org.tfeb.star.ranges" :report *test-report-class*))
