;;;; Štar simple benchmarks
;;;

(in-package :org.tfeb.star/bench)

(defun list/star (l)
  (declare (type list l)
           (optimize speed))
  (let ((i 0))
    (declare (type fixnum i))
    (for* ((_ (in-list l))
           (_ (in-list l))
           (_ (in-list l)))
      (incf i))
    i))

(defun list/loop (l)
  (declare (type list l)
           (optimize speed))
  (let ((i 0))
    (declare (type fixnum i))
    (loop for a in l
          do (loop for b in l
                   do (loop for c in l
                            do (incf i))))
    i))

(defun list/dolist (l)
  (declare (type list l)
           (optimize speed))
  (let ((i 0))
    (declare (type fixnum i))
    (dolist (a l)
      (dolist (b l)
        (dolist (c l)
          (incf i))))
    i))

(defun bench-list (n)
  (let ((l (make-list n)))
    (reporting-times (("** lists of length ~D, nesting 3" n))
      (star (list/star l))
      (loop (list/loop l))
      (dolist (list/dolist l)))))

(defun range/star/in-naturals (n)
  (declare (type fixnum n)
           (optimize speed))
  (let ((m 0))
    (declare (type fixnum m))
    (for* (((i :type fixnum) (in-naturals :bound n :fixnum t))
           ((j :type fixnum) (in-naturals :bound n :fixnum t)))
      (setf m (max i j m)))
    m))

(defun range/star/stepping (n)
  (declare (type fixnum n)
           (optimize speed))
  (let ((m 0))
    (declare (type fixnum m))
    (for* (((i :type fixnum) (stepping (i :type fixnum
                                          :initially 0 :then (1+ i)
                                          :while (< i n))))
           ((j :type fixnum) (stepping (i :type fixnum
                                          :initially 0 :then (1+ i)
                                          :while (< i n)))))
      (setf m (max i j m)))
    m))

(defun range/loop (n)
  (declare (type fixnum n)
           (optimize speed))
  (let ((m 0))
    (declare (type fixnum m))
    (loop for i of-type fixnum below n
          do (loop for j of-type fixnum below n
                   do (setf m (max i j m))))
    m))

(defun range/dotimes (n)
  (declare (type fixnum n)
           (optimize speed))
  (let ((m 0))
    (declare (type fixnum m))
    (dotimes (i n)
      (declare (type fixnum i))
      (dotimes (j n)
        (declare (type fixnum j))
        (setf m (max i j m))))
    m))

(defun bench-range (n)
  (declare (type fixnum n))
  (reporting-times (("** range ~D, nesting 2" n))
      (star/in-naturals (range/star/in-naturals n))
      (star/stepping (range/star/stepping n))
      (loop (range/loop n))
      (dotimes (range/dotimes n))))

(define-benchmark simple ("Simple")
  (bench-list 2000)
  (bench-range 100000))
