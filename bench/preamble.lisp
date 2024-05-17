;;;; Štar benchmarks
;;;

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
  (format to "~&~A~%~50A ~10@A ~10@A~%"
          header "what" "seconds" "ratio")
  (let ((times (timing-equivalent-thunks thunks :test test)))
    (when (not (null times))
      (when (some #'zerop times)
        (error "too fast"))
      (let ((first-time (first times)))
        (for ((tag (in-list tags))
              (time (in-list times)))
          (format to "~&~50A ~10,3F ~10,3F~%"
                  tag
                  (->s time)
                  (float (/ time first-time) 1.0d0)))))
    times))

(defmacro reporting-times ((header &key (to '*standard-output*) (test '(function eql)))
                                   &body tags/forms)
  (multiple-value-bind (tags thunks)
      (with-collectors (tag thunk)
        (dolist (tag/form tags/forms)
          (tag (first tag/form))
          (thunk `(lambda () ,(second tag/form)))))
  `(reporting-tag/thunk-times
    ,(etypecase header
       (string
        header)
       (list `(format nil ,@header)))
    '(,@tags)
    (list ,@thunks)
    :to ,to
    :test ,test)))

(defvar *benchmarks* '())

(defmacro define-benchmark (name (&optional description) &body forms)
  `(let ((thunk (lambda () ,@forms))
         (found (assoc ',name *benchmarks*)))
     (if found
         (setf (cdr found) (list ',description thunk))
       (push (list ',name ',description thunk) *benchmarks*))
     ',name))

(defun run-benchmarks (&key (names t) (to *standard-output*))
  (let ((*standard-output* to))
    (for ((selected (in-list  (collecting
                              (for ((bm (in-list (reverse *benchmarks*))))
                                (when (or (eq names t)
                                          (member (first bm) names))
                                  (collect bm)))))))
    (destructuring-bind (name description thunk) selected
      (format t "~&* ~A~%" (or description name))
      (funcall thunk))))
  (length *benchmarks*))
