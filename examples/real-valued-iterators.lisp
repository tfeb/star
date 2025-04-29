;;;; Zyni's macro for defining iterators over real types
;;;
;;; This needs Štar 5
;;;
;;; It was too hard to write the version of ENSURIFY for the
;;; optimizers, so this is a bit horrible: sorry.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.star
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.utilities)
  :compile t))

(defpackage :org.tfeb.star/examples/real-valued-iterators
  (:use :cl)
  (:use
   :org.tfeb.star
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.utilities)
  (:export
   #:define-real-valued-iterators))

(in-package :org.tfeb.star/examples/real-valued-iterators)

(defmacro define-real-valued-iterators (&body types/names &environment e)
  "Define optimized iterators for some subtypes of REAL

Each type/name may either be a symbol naming a subtype of REAL, or a
list (type-name iterator-name).  The second case is useful, for
instance, for type names which are not symbols.

Types are checked to be recognizable subtypes of REAL in the
compilation environment."
  `(progn
     ,@(collecting
         (for ((type/name (in-list types/names)))
           (multiple-value-bind (type name)
               (destructuring-match type/name
                 (type
                  (:when (symbolp type))
                  (values type (symbolify *package* "IN-" type "S")))
                 ((type name)
                  (:when (symbolp name))
                  (values type name))
                 (otherwise
                  (star-error "bad type/name ~S" type/name)))
             (assert (subtypep type 'real e) (type)
               "~S is not a recognizable subtype of ~S" type 'real)
             (assert (or (subtypep type 'float e)
                         (subtypep type 'integer e)
                         (subtypep type 'ratio e))
                 (type)
               "~S is an unexpected real type" type)
             (collect
              `(defun ,name (&optional v1 v2)
                 (declare (type (or real null) v1 v2))
                 (macrolet ((ensurify (thing)
                              ,(cond
                                ((subtypep type 'float e)
                                 ``(coerce ,thing ',',type))
                                ((subtypep type 'integer e)
                                 '`(ceiling ,thing))
                                ((subtypep type 'ratio e)
                                 '`(rationalize ,thing)))))
                   (multiple-value-bind (start bound) (cond
                                                       ((and v1 v2)
                                                        (values v1 v2))
                                                       (v2
                                                        (values 0 v2))
                                                       (t
                                                        (values 0 nil)))
                     (declare (type real start) (type (or real null) bound))
                     (typecase bound
                       (null
                        (let ((v (ensurify start)))
                          (values
                           (constantly t)
                           (thunk (prog1 v (incf v))))))
                       (real
                        (let ((v (ensurify start))
                              (b (ensurify bound)))
                          (values
                           (thunk (< v b))
                           (thunk (prog1 v (incf v)))))))))))
            (collect
             ;; I just gave up here with the fancy macro you need,
             ;; because it's too fiddly.  So there are three
             ;; essentially identical copies of this.
             (cond
              ((subtypep type 'float e)
               `(define-iterator-optimizer ,name (form)
                  (destructuring-match form
                  ((_)
                   (with-names (<v>)
                     (values
                      t
                      `(((,<v>) ,(coerce 0 ',type) (declare (type ,',type ,<v>))))
                      t
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  ((_ bound)
                   (with-names (<v> <b>)
                     (values
                      t
                      `(((,<v> ,<b>) (values ,(coerce 0 ',type)
                                             (coerce ,bound ',',type))
                         (declare (type ,',type ,<v> ,<b>))))
                      `(< ,<v> ,<b>)
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  ((_ start bound)
                   (with-names (<v> <b>)
                     (values
                      t
                      `(((,<v> ,<b>) (values (coerce ,start ',',type)
                                             (coerce ,bound ',',type))
                         (declare (type ,',type ,<v> ,<b>))))
                      `(< ,<v> ,<b>)
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  (otherwise
                   (star-error "What even is ~S?" form)))))
              ((subtypep type 'integer e)
               `(define-iterator-optimizer ,name (form)
                  (destructuring-match form
                    ((_)
                     (with-names (<v>)
                       (values
                        t
                        `(((,<v>) 0 (declare (type ,',type ,<v>))))
                      t
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  ((_ bound)
                   (with-names (<v> <b>)
                     (values
                      t
                      `(((,<v> ,<b>) (values 0 (ceiling ,bound))
                         (declare (type ,',type ,<v> ,<b>))))
                      `(< ,<v> ,<b>)
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  ((_ start bound)
                   (with-names (<v> <b>)
                     (values
                      t
                      `(((,<v> ,<b>) (values (ceiling ,start)
                                             (ceiling ,bound))
                         (declare (type ,',type ,<v> ,<b>))))
                      `(< ,<v> ,<b>)
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  (otherwise
                   (star-error "What even is ~S?" form)))))
              ((subtypep type 'ratio e)
               `(define-iterator-optimizer ,name (form)
                  (destructuring-match form
                    ((_)
                     (with-names (<v>)
                       (values
                        t
                        `(((,<v>) 0 (declare (type ,',type ,<v>))))
                      t
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  ((_ bound)
                   (with-names (<v> <b>)
                     (values
                      t
                      `(((,<v> ,<b>) (values 0 (rationalize ,bound))
                         (declare (type ,',type ,<v> ,<b>))))
                      `(< ,<v> ,<b>)
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  ((_ start bound)
                   (with-names (<v> <b>)
                     (values
                      t
                      `(((,<v> ,<b>) (values (rationalize ,start)
                                             (rationalize ,bound))
                         (declare (type ,',type ,<v> ,<b>))))
                      `(< ,<v> ,<b>)
                      `(prog1 ,<v> (incf ,<v>))
                      '(:types (,type)))))
                  (otherwise
                   (star-error "What even is ~S?" form))))))))))))
