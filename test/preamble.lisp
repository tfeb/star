;;;; Test preamble
;;;

(in-package :org.tfeb.star/test)

(defvar *test-report-class* 'summary)
(defvar *test-individually* nil)

(define-test "org.tfeb.star")

(defun pessimize-maybe-wrapped-for-expression (maybe-wrapped-for-expression)
  ;; MAYBE-WRAPPED-FOR-EXPRESSION is either a for expression or of the
  ;; form (x ...) where any of the ...'s may be (FOR (...) ...).  This
  ;; is not a code walker!
  (flet ((pessimize (for-expression)
           (destructuring-match for-expression
             ((for (&rest clauses) &body forms)
              (:when (literals for))
              (multiple-value-bind (bindings rewritten-clauses)
                  (with-collectors (binding clause)
                    (for ((clause (in-list clauses)))
                      (destructuring-match clause
                        ((var/s iterator)
                         (with-names (<valid> <cursor>)
                           (binding `((,<valid> ,<cursor>) ,iterator))
                           (clause `(,var/s (values ,<valid> ,<cursor>)))))
                        (otherwise
                         (error "bad clause ~S" clause)))))
                (iterate next-binding ((btail bindings))
                  (if (null btail)
                      `(for ,rewritten-clauses
                         ,@forms)
                    `(multiple-value-bind ,@(first btail)
                         ,(next-binding (rest btail)))))))
             (otherwise
              (error "not a for expression: ~S" for-expression)))))
    (destructuring-match maybe-wrapped-for-expression
      ((for . _)
       (:when (literals for))
       (pessimize maybe-wrapped-for-expression))
      ((op &rest possibles)
       (:when (symbolp op))
       `(,op
         ,@(collecting
             (for ((possible (in-list possibles)))
               (destructuring-match possible
                 ((for . _)
                  (:when (literals for))
                  (collect (pessimize possible)))
                 (otherwise
                  (collect possible))))))))))

(defmacro pessimizing-for (((p o &optional e) maybe-wrapped-for-expression) &body forms)
  ;; P & O will be functions corresponding to pessimal and optimal
  ;; versions of FOR-EXPRESSION in BODY respectively.  E is the
  ;; expression itself for error reporting
  (if e
      `(let ((,e ',maybe-wrapped-for-expression))
         (declare (ignorable e))
         (flet ((,p ()
                  ,(pessimize-maybe-wrapped-for-expression
                    maybe-wrapped-for-expression))
                (,o ()
                  ,maybe-wrapped-for-expression))
           ,@forms))
    `(flet ((,p ()
              ,(pessimize-maybe-wrapped-for-expression
                maybe-wrapped-for-expression))
            (,o ()
              ,maybe-wrapped-for-expression))
       ,@forms)))

(defun repeat (n)
  ;; Used to ensure termination
  (let ((i 0))
    (values
     (lambda ()
       (< i n))
     (lambda ()
       (prog1 i
         (incf i))))))
