;;;; Test collecting vs loop / collect
;;;

(in-package :org.tfeb.star/bench)


(defun loop-collect (n m)
  (declare (type fixnum n m)
           (optimize speed))
  (loop repeat n
        do
        (loop repeat m
              collect 1))
  t)

(defun for-collecting (n m)
  (declare (type fixnum n m)
           (optimize speed))
  (for ((_ (in-range :from 0 :before n :by 1 :type 'fixnum)))
    (collecting
      (for ((_ (in-range :from 0 :before m :by 1 :type 'fixnum)))
        (collect 1))))
  t)

(defun bench-collecting (n m)
  (declare (type fixnum n m))
  (reporting-times (("** Collecting: ~D iterations of lists of length ~D" n m))
    (for (for-collecting n m))
    (loop (loop-collect n m))))

(define-benchmark collecting ("Collecting")
  (bench-collecting 10000 100000))
