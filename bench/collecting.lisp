;;;; Štar collecting benchmarks
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
  (for ((_ (in-naturals :bound n :fixnum t)))
    (collecting
      (for ((_ (in-naturals :bound m :fixnum t)))
        (collect 1))))
  t)

(defun bench-collecting (n m)
  (declare (type fixnum n m))
  (reporting-times (("** Collecting: ~D iterations of lists of length ~D" n m))
    (for (for-collecting n m))
    (loop (loop-collect n m))))

(defun loop-summing (n m)
  (declare (type fixnum n m)
           (optimize speed))
  (loop repeat n
        summing (loop repeat m
                       summing 1 fixnum)
          fixnum))

(defun for-summing (n m)
  (declare (type fixnum n m)
           (optimize speed))
  (with-accumulators ((r + :initially 0 :type fixnum))
    (for ((_ (in-naturals :bound n :fixnum t)))
      (r
       (with-accumulators ((s + :initially 0 :type fixnum))
         (for ((_ (in-naturals :bound m :fixnum t)))
           (s 1)))))))

(defun dotimes-summing (n m)
  (declare (type fixnum n m)
           (optimize speed))
    (with-accumulators ((r + :initially 0 :type fixnum))
      (dotimes (i n)
        (declare (type fixnum i)
                 (ignorable i))
        (r
         (with-accumulators ((s + :initially 0 :type fixnum))
                 (dotimes (j m)
                   (declare (type fixnum j)
                            (ignorable j))
                   (s 1)))))))

(defun bench-summing (n m)
  (declare (type fixnum n m))
  (reporting-times (("** Summing: ~D iterations of ~D iterations" n m))
    (for (for-summing n m))
    (loop (loop-summing n m))
    (dotimes (dotimes-summing n m))))

(define-benchmark collecting ("Collecting")
  (bench-collecting 10000 100000)
  (bench-summing 100000 100000))
