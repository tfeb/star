;;;; Štar unroll support
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (("pkg")
  :compile t))

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:provides :org.tfeb.star/unroll)

#-org.tfeb.tools.require-module
(provide :org.tfeb.star/unroll)

(in-package :org.tfeb.star/unroll/impl)

(define-condition unroll-syntax-error (star-syntax-error)
  ())

(defun unroll-syntax-error (form control &rest arguments)
  (error 'unroll-syntax-error
         :form form
         :format-control control
         :format-arguments arguments))

;;;; Shim and its control
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; So we can use the shim here
  (defparameter *enable-unrolling* nil
    "If true, iterators may be unrolled

See *UNROLL-ITERATORS-BY*"))

(defconstant for/star 'org.tfeb.*:for)

(defmacro for ((&rest clauses) &body decls/forms)
  "A variant of FOR which may try to unroll loops

If *ENABLE-UNROLLING* is true at macro expansion time this will try to
unroll loops by *UNROLL-BY*.  If it is false it is just FOR."
  `(,(if *enable-unrolling* 'for/unroll for/star)
    ,clauses ,@decls/forms))

(defvar *unroll-by* 4
  "How many times to unroll loops

This matters at macroexpansion time")

;;;; Unroller protocol
;;;
;;; This is close to a clone of the way iterator optimizers are
;;; defined, which see.
;;;
;;; Unrollers are called with
;;; - the iterator form being unrolled
;;; - a list of variables where each variable is either the name of a
;;;   variable or NIL for an anonymous varable
;;; - how many steps to unroll by
;;; - the environment
;;;  - the stack
;;; The last two arguments can be omitted in definitions as for optimizers
;;;
;;; Unrollers return
;;; - whether they unrolled the iterator
;;; - a list of binding sets which are the same as for optimizers
;;; - the new iterator form
;;; - a list of lists of stepper forms which should be one less in
;;; length than the amount to unroll by
;;;

(defun make-iterator-unroller-table (&optional (from nil))
  "Make an iterator unroller table

These are probably just hash-tables, but this means you don't have to
assume they are, and they can be suitably weak where implementations
support that"
  (let ((table #+lispWorks (make-hash-table :weak-kind ':key)
               #+SBCL (make-hash-table :weakness ':key)
               #-(or LispWorks SBCL) (make-hash-table)))
    (when from
      (maphash (lambda (k v)
                 (setf (gethash k table) v))
               from))
    table))

(defvar *iterator-unrollers*
  ;; Unlike iterator optimizers there is no builtin table at the base
  ;; of the stack
  (list (make-iterator-unroller-table))
  "The stack of iterator-unroller tables")

(defun remove-iterator-unroller (name table)
  "Remove an unroller named NAME from TABLE

It is not an error if it is not present."
  (remhash name table))

(defun get-iterator-unroller (name table)
  "Return an unroller with name NAME in TABLE.

Second value is NIL if it is not present.

This is an accessor."
  (gethash name table))

(defun (setf get-iterator-unroller) (new name table)
  (setf (gethash name table) new))

(defun map-iterator-unroller-table (function table)
  "Map FUNCTION over TABLE.

FUNCTION takes two arguments, the unroller name and the unroller.
It is allowed to call REMOVE-ITERATOR-UNROLLER on the unroller, but
not on any other one."
  (maphash function table))

(defun find-iterator-unroller (name &optional (unrollers *iterator-unrollers*))
  "Find an iterator unroller in the stack

Returns the unroller, and the tail of the stack where it was found, or NIL and NIL."
  (destructuring-match unrollers
    (()
     (values nil nil))
    ((table . more)
     (multiple-value-bind (found foundp) (get-iterator-unroller name table)
       (if foundp
           (values found unrollers)
         (find-iterator-unroller name more))))
    (otherwise
     (star-error "unroller stack isn't"))))

(defmacro define-iterator-unroller (name/table (form-name vars-name unroll-by-name
                                                          &optional
                                                          (env-name nil env-name-p)
                                                          (stack-name nil stack-name-p))
                                                &body forms)
  "Define an iterator unroller for Štar

See the manual"
  (let-values (((name table)
                (destructuring-match name/table
                  ((name table)
                   (:when (symbolp name))
                   (values name table))
                  (name
                   (:when (symbolp name))
                   (values name '(car *iterator-unrollers*)))
                  (otherwise
                   (star-syntax-error name/table "bad name / table"))))
               ((decls body) (parse-simple-body forms)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; There is a question about whether unrollers should be
       ;; defined before their functions: previously they were not,
       ;; now they are.  See the manual.
       (setf (get-iterator-unroller ',name ,table)
             ,(cond
               (stack-name-p
                `(lambda (,form-name ,vars-name ,unroll-by-name &optional ,env-name ,stack-name)
                   ,@decls
                   (block ,name
                     ,@body)))
               (env-name-p
                (with-names (<ignored-stack>)
                  `(lambda (,form-name ,vars-name ,unroll-by-name
                                       &optional ,env-name ,<ignored-stack>)
                     (declare (ignore ,<ignored-stack>))
                     ,@decls
                     (block ,name
                       ,@body))))
               (t
                (with-names (<ignored-env> <ignored-stack>)
                  `(lambda (,form-name ,vars-name ,unroll-by-name
                                       &optional ,<ignored-env> ,<ignored-stack>)
                     (declare (ignore ,<ignored-env> ,<ignored-stack>))
                     ,@decls
                     (block ,name
                       ,@body))))))
       ',name)))

;;;; Parsing clauses for unrolling
;;;

(defun iteration-variables (var/s)
  ;; return a list of variable names or NIL for anonyous variable
  ;; placeholders
  (destructuring-match var/s
    (()
     var/s)
    (var
     (:when (symbolp var))
     (if (anonymous-variable-p var) '(nil) (list var)))
    ((var &key anonymous &allow-other-keys)
     (:when (symbolp var))
     (if (or anonymous (anonymous-variable-p var)) '(nil) (list var)))
    ((&rest varspecs)
     (collecting
       (for ((varspec (in-list varspecs)))
         (destructuring-match varspec
           ((var &key anonymous &allow-other-keys)
            (:when (symbolp var))
            (collect (if (or anonymous (anonymous-variable-p var)) nil var)))
           (var
            (:when (symbolp var))
            (collect (if (anonymous-variable-p var) nil var)))
           (otherwise
            (unroll-syntax-error var/s "Mutant variables"))))))
    (otherwise
     (unroll-syntax-error var/s "Completely mutant variables"))))

(defun unrollers-for-clauses (clauses environment &key (unroll-by *unroll-by*)
                                      (stack *iterator-unrollers*))
  ;; Return T and a list of binding sets, a list of new clauses, and the steppers
  ;; or NIL, NIL, NIL, NIL on failure
  (flet ((fail ()
           (return-from unrollers-for-clauses (values nil '() '() '()))))
    (multiple-value-bind (binding-sets new-clauses stepper-sets)
        (with-collectors (binding-set new-clause steppers)
          (for ((clause (in-list clauses)))
            (destructuring-match clause
              ((var/s (&whole form iterator &rest _))
               (:when (symbolp iterator))
               (let ((vars (iteration-variables var/s))
                     (unroller (find-iterator-unroller iterator)))
                 (unless unroller (fail))
                 (multiple-value-bind (unrollable binding-sets new-iterator steppers)
                     (funcall unroller form vars unroll-by environment stack)
                   (unless unrollable (fail))
                   (for ((binding-set (in-list binding-sets)))
                     (binding-set binding-set))
                   (new-clause `(,var/s ,new-iterator))
                   (steppers steppers)))))))
      (values t binding-sets new-clauses (collecting
                                           (for ((u (in-naturals (1- unroll-by))))
                                             (collect
                                              (collecting
                                                (for* ((stepper-set (in-list stepper-sets))
                                                       (stepper (in-list (nth u stepper-set))))
                                                  (collect stepper))))))))))

;;;; The macro
;;;

(defun expand-for/unroll (clauses body environment)
  (let-values (((declarations forms) (parse-simple-body body))
               ((unrollable binding-sets new-clauses steppers)
                (unrollers-for-clauses clauses environment)))
    (if unrollable
        (iterate expand-bindings ((btail binding-sets))
          (destructuring-match btail
            (((variables form &rest declarations) . more)
             `(multiple-value-bind ,variables ,form
                ,@declarations
                ,(expand-bindings more)))
            (()
             `(,for/star ,new-clauses
                ,@declarations
                ,@(collecting
                    (for* ((stepper (in-list steppers))
                           (form (in-iterators (in-list forms) (in-list stepper))))
                      (collect form))
                    (for ((form (in-list forms)))
                      (collect form)))))))
      (progn
        (when *enable-unrolling*
          (star-note "couldn't unroll ~A" `(for ,clauses ,@body)))
        `(,for/star ,clauses ,@body)))))

(defmacro for/unroll ((&rest clauses) &body body &environment environment)
  "A variant of FOR which will always try to unroll loops by *UNROLL-BY*"
  (expand-for/unroll clauses body environment))
