;;;; Štar: some common things
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ("pkg" :compile t))

(in-package :org.tfeb.star/common)

;;;; Conditions
;;;
;;; I am not sure the classification here is right

(define-condition star-error (simple-error)
  ;; Any of these are not our fault
  ()
  (:documentation
   "Condition type for Štar's errors"))

(defun star-error (control &rest arguments)
  (error 'star-error
         :format-control control
         :format-arguments arguments))

(define-condition star-syntax-error (star-error program-error)
  ((form :initform nil
         :initarg :form
         :reader star-syntax-error-form))
  (:report
   (lambda (se stream)
     (format stream "Syntax error~@[ in ~S~]: ~A"
             (star-syntax-error-form se)
             (apply #'format nil
                    (simple-condition-format-control se)
                    (simple-condition-format-arguments se))))))

(defun star-syntax-error (form control &rest arguments)
  (error 'star-syntax-error
         :form form
         :format-control control
         :format-arguments arguments))

(define-condition catastrophe (simple-error)
  ;; These definitely are our fault
  ())

(defun catastrophe (control &rest arguments)
  (error 'catastrophe
         :format-control control
         :format-arguments arguments))

(define-condition star-note (simple-condition)
  ()
  (:documentation
   "Condition type for Štar compilation notes"))

(defun star-note (format &rest arguments)
  (signal 'star-note
          :format-control format
          :format-arguments arguments))

(defmacro reporting-star-notes ((&optional (to '*debug-io*)) &body forms)
  "Report any Štar notes to TO"
  (with-names (<to>)
    `(let ((,<to> ,to))
       (handler-bind ((star-note
                       (lambda (note)
                         (format ,<to> "~&Note: ~A~%" note))))
         ,@forms))))
