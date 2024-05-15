;;;; Štar benchmarks
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ("pkg" :compile t))

(in-package :org.tfeb.star/bench)

(defmacro timing-equivalent-forms ((&rest kws &key &allow-other-keys)
                                   &body forms)
  `(timing-equivalent-thunks
    (list ,@(mapcar (lambda (form)
                      `(lambda () ,form))
                    forms))
    ,@kws))

(defun timing-equivalent-thunks (thunks &key (test #'eql))
  (multiple-value-bind (tms vals)
      (with-collectors (tm val)
        (dolist (thunk thunks)
          (let ((start (get-internal-real-time)))
            (val (funcall thunk))
            (tm (- (get-internal-real-time) start)))))
    (iterate check ((v (first vals)) (vt (rest vals)))
      (unless (null vt)
        (destructuring-bind (vv . vtt) vt
          (unless (funcall test v vv)
            (error "values differ"))
          (check vv vtt))))
    tms))

(defun ->s (it)
  (float (/ it internal-time-units-per-second)
         1.0d0))

(defun reporting-tag/thunk-times (header tags thunks &key (test #'eql) (to *standard-output*))
  (let ((times (timing-equivalent-thunks thunks :test test)))
    (when (not (null times))
      (when (some #'zerop times)
        (error "too fast"))
      (format to "~&~A~%~50A ~10@A ~10@A~%"
              header "what" "seconds" "ratio")
      (let ((first-time (first times)))
        (for ((tag (in-list tags))
              (time (in-list times)))
          (format to "~&~50S ~10,3F ~10,3F~%"
                  tag
                  (->s time)
                  (float (/ time first-time) 1.0d0)))))
    times))

(defmacro reporting-times (header &body tags/forms)
  (multiple-value-bind (tags thunks)
      (with-collectors (tag thunk)
        (dolist (tag/form tags/forms)
          (tag (first tag/form))
          (thunk `(lambda () ,(second tag/form)))))
  `(reporting-tag/thunk-times
    ,header
    '(,@tags)
    (list ,@thunks))))

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
    (reporting-times (format nil "** lists of length ~D, nesting 3" n)
      (star (list/star l))
      (loop (list/loop l))
      (dolist (list/dolist l)))))

(defun range/star/with-step (n)
  (declare (type fixnum n)
           (optimize speed))
  (let ((m 0))
    (declare (type fixnum m))
    (for* (((i :type fixnum) (in-range :from 0 :before n :by 1 :type 'fixnum))
           ((j :type fixnum) (in-range :from 0 :before n :by 1 :type 'fixnum)))
      (setf m (max i j m)))
    m))

(defun range/star/no-step (n)
  (declare (type fixnum n)
           (optimize speed))
  (let ((m 0))
    (declare (type fixnum m))
    (for* (((i :type fixnum) (in-range :from 0 :before n :type 'fixnum))
           ((j :type fixnum) (in-range :from 0 :before n :type 'fixnum)))
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
        (setf m (max m i j))))
    m))

(defun bench-range (n)
  (declare (type fixnum n))
  (reporting-times (format nil "** range ~D, nesting 2" n)
      (star/with-step (range/star/with-step n))
      (star/no-step (range/star/with-step n))
      (loop (range/loop n))
      (dotimes (range/dotimes n))))

;;;; Run the benchmarks
;;;
(format t "~&* Running benchmarks~%")
(bench-list 2000)
(bench-range 100000)
