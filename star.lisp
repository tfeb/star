;;;; Štar
;;; New improved simpler version
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (("pkg" "common")
  :compile t))

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:provides :org.tfeb.star)

#-org.tfeb.tools.require-module
(provide :org.tfeb.star)

(in-package :org.tfeb.star/impl)

;;;; Iterator protocol
;;;
;;; An iterator optimizer returns five values for an iterator
;;; - true if it has succeeded
;;; - a list of binding sets (see below)
;;; - a form which returns true if the cursor is valid
;;; - the cursor, which returns the next values
;;; - a wrapper function, or NIL which is called with the entire form
;;;
;;; A binding set is a list of two or more elements:
;;; - variables to bind (for MULTIPLE-VALUE-BIND)
;;; - a form
;;; - zero or more declarations
;;;

(defun make-iterator-optimizer-table (&optional (from nil))
  "Make an iterator optimizer table

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

(defvar *builtin-iterator-optimizer-table*
  (make-iterator-optimizer-table))

(defparameter *star-bootstrap* t)

(defvar *iterator-optimizers*
  (list (make-iterator-optimizer-table)
        *builtin-iterator-optimizer-table*)
  "The stack of iterator optimizer tables")

(defvar *enable-iterator-optimizers* t
  "If true, iterator optimizers will be called at macroexpansion time")

(defun remove-iterator-optimizer (name table)
  "Remove an optimizer named NAME from TABLE

It is not an error if it is not present."
  (unless *star-bootstrap*
    (when (eq table *builtin-iterator-optimizer-table*)
      (restart-case
          (star-error "Deleting optimizer from builtin table")
        (continue ()
          :report "do it anyway"))))
  (remhash name table))

(defun get-iterator-optimizer (name table)
  "Return an optimizer with name NAME in TABLE.

Second value is NIL if it is not present.

This is an accessor."
  (gethash name table))

(defun (setf get-iterator-optimizer) (new name table)
  (unless *star-bootstrap*
    (when (eq table *builtin-iterator-optimizer-table*)
      (restart-case
          (star-error "Modifying optimizer from builtin table")
        (continue ()
          :report "do it anyway"))))
  (setf (gethash name table) new))

(defun map-iterator-optimizer-table (function table)
  "Map FUNCTION over TABLE.

FUNCTION takes two arguments, the optimizer name and the optimizer.
It is allowed to call REMOVE-ITERATOR-OPTIMIZER on the optimizer, but
not on any other one."
  (maphash function table))

(defun find-iterator-optimizer (name &optional (optimizers *iterator-optimizers*))
  "Find an iterator optimizer in the stack

Returns the optimizer, and the tail of the stack where it was found, or NIL and NIL."
  (destructuring-match optimizers
    (()
     (values nil nil))
    ((table . more)
     (multiple-value-bind (found foundp) (get-iterator-optimizer name table)
       (if foundp
           (values found optimizers)
         (find-iterator-optimizer name more))))
    (otherwise
     (star-error "optimizer stack isn't"))))

(defmacro define-iterator-optimizer (name/table (form-name &optional
                                                           (env-name nil env-name-p)
                                                           (stack-name nil stack-name-p))
                                                &body forms)
  "Define an iterator optimizer for Štar

See the manual"
  (multiple-value-bind (name table)
      (destructuring-match name/table
        ((name table)
         (:when (symbolp name))
         (values name table))
        (name
         (:when (symbolp name))
         (values name '(car *iterator-optimizers*)))
        (otherwise
         (syntax-error name/table "bad name / table")))
    (multiple-value-bind (decls body) (parse-simple-body forms)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; There is a question about whether optimizers should be
         ;; defined before their functions: previously they were not,
         ;; now they are.  See the manual.
         (setf (get-iterator-optimizer ',name ,table)
               ,(cond
                 (stack-name-p
                  `(lambda (,form-name &optional ,env-name ,stack-name)
                     ,@decls
                     (block ,name
                       ,@body)))
                 (env-name-p
                  (with-names (<ignored-stack>)
                    `(lambda (,form-name &optional ,env-name ,<ignored-stack>)
                       (declare (ignore ,<ignored-stack>))
                       ,@decls
                       (block ,name
                         ,@body))))
                 (t
                  (with-names (<ignored-env> <ignored-stack>)
                    `(lambda (,form-name &optional ,<ignored-env> ,<ignored-stack>)
                       (declare (ignore ,<ignored-env> ,<ignored-stack>))
                       ,@decls
                       (block ,name
                         ,@body))))))
         ',name))))

(defun iterator-optimizers (iterator environment)
  ;; Return found values which explain how to optimize an iterator: a
  ;; list of binding sets, a valid form a cursor form and a wrapper, or NIL
  (flet ((fallback-optimizers (valid-name cursor-name)
           ;; Note this only returns three forms, because it's the
           ;; fallback: it can't decline to handle it.
           (values
            ;; binding set
            `(((,valid-name ,cursor-name)
               ,iterator
               (declare (type function ,valid-name ,cursor-name))))
            ;; form returning true if more
            `(funcall ,valid-name)
            ;; form returning values
            `(funcall ,cursor-name)
            ;; no wrapper
            nil)))
    (destructuring-match iterator
      ((f &rest _)
       (:when (symbolp f))
       (if *enable-iterator-optimizers*
           (multiple-value-bind (found stack) (find-iterator-optimizer f)
             (if found
                 (multiple-value-bind (handled binding-sets valid cursor wrapper)
                     (funcall found iterator environment stack)
                   (if handled
                       (values binding-sets valid cursor wrapper)
                     (fallback-optimizers
                      (make-symbol (format nil "~A-VALID" (symbol-name f)))
                      (make-symbol (format nil "~A-CURSOR" (symbol-name f))))))
               (fallback-optimizers
                (make-symbol (format nil "~A-VALID" (symbol-name f)))
                (make-symbol (format nil "~A-CURSOR" (symbol-name f))))))
         (fallback-optimizers
          (make-symbol (format nil "~A-VALID" (symbol-name f)))
          (make-symbol (format nil "~A-CURSOR" (symbol-name f))))))
      (otherwise
       (fallback-optimizers (make-symbol "VALID") (make-symbol "CURSOR"))))))

;;;; Štar itself
;;;

;;; Parsing clause descriptions
;;;
;;; We need to make sure iterators are unique objects, hence this hair
;;; of boxing them (could just wrap it in a cons of course)
;;;

(defstruct (unique-iterator
            (:constructor unique-iterator (iterator)))
  iterator)

(defstruct var
  (name (catastrophe "no name for unique iterator"))
  (type t)
  (special-p nil)
  (ignore-p nil)
  (ignorable-p nil)
  (anonymous-p nil)
  (declarations '()))

(defstruct clause
  (vars (catastrophe "no vars"))
  (unique-iterator (catastrophe "no iterator")))

(defun anonymous-variable-p (s)
  (string= (symbol-name s) "_"))

(defun parse-clause-description (clause-description)
  ;; Parse one clause description returning a CLAUSE, or NIL
  (flet ((parse-complex-variable (variable args)
           (destructuring-match args
             ((&key (type t) (anonymous nil anonymousp) (special nil)
                    (ignore nil) (ignorable nil)
                    (declarations '()))
              (make-var :name variable
                        :type type
                        :special-p special
                        :ignore-p ignore
                        :ignorable-p ignorable
                        :anonymous-p (if anonymousp
                                         anonymous
                                       (anonymous-variable-p variable))
                        :declarations declarations))
             (otherwise
              (return-from parse-clause-description nil)))))
    (destructuring-match clause-description
      ((variable iterator)
       (:when (symbolp variable))
       (make-clause :vars (list (make-var :name variable
                                          :anonymous-p (anonymous-variable-p variable)))
                    :unique-iterator (unique-iterator iterator)))
      (((variable &rest args &key name type anonymous special ifnore ignorable declarations)
        iterator)
       (:when (symbolp variable))
       (declare (ignore name type anonymous special ifnore ignorable declarations))
       (make-clause :vars (list (parse-complex-variable variable args))
                    :unique-iterator (unique-iterator iterator)))
      (((&rest vardescs) iterator)
       (make-clause :vars (mapcar (lambda (vardesc)
                                    (destructuring-match vardesc
                                      (variable
                                       (:when (symbolp variable))
                                       (make-var :name variable
                                                 :anonymous-p (anonymous-variable-p variable)))
                                      ((variable &rest args &key &allow-other-keys)
                                       (:when (symbolp variable))
                                       (parse-complex-variable variable args))
                                      (otherwise
                                       (return-from parse-clause-description nil))))
                                  vardescs)
                    :unique-iterator (unique-iterator iterator)))
      (otherwise nil))))

(defun parse-clause-descriptions (clause-descriptions)
  ;; return either NIL and NIL if things are hopeless, or NIL and a
  ;; list of offending clause descriptions, or T and the parsed
  ;; descriptions
  (multiple-value-bind (goods bads)
      (with-collectors (good bad)
        (iterate next ((ctail clause-descriptions))
          (typecase ctail
            (null nil)
            (cons
             (destructuring-bind (this . more) ctail
               (let ((parsed (parse-clause-description this)))
                 (if parsed
                     (good parsed)
                   (bad this))
                 (next more))))
            (t
             (return-from parse-clause-descriptions (values nil nil))))))
    (if bads
        (values nil bads)
      (values t goods))))

(defstruct let-binding
  (clauses '()))

(defstruct multiple-value-binding
  (clause (catastrophe "need a clause")))

(defun clauses->bindings (clauses)
  ;; Coalesce a bunch of clauses into corresponding binding objects
  (collecting
    (iterate next ((ctail clauses)
                   (current '()))
      (if (null ctail)
          (unless (null current)
            (collect (make-let-binding :clauses (nreverse current))))
        (destructuring-bind (this . more) ctail
          (case (length (clause-vars this))
            (0
             (catastrophe "empty binding?"))
            (1
             (next more (cons this current)))
            (otherwise
             (unless (null current)
               (collect (make-let-binding :clauses (nreverse current))))
             (collect (make-multiple-value-binding :clause this))
             (next more '()))))))))

(defstruct (itable-entry
            (:conc-name "IE-"))
  ;; an entry in the alist from unique iterator to information for it.
  valid-form
  cursor-form)

(defun call/compiling-iterator-optimizers (f unique-iterators environment)
  ;; call F, which should return a single form, returning its result
  ;; wrapped in suitable bindings for the optimizers F is called with
  ;; an alist which maps from unique-iterators to itable-entries.
  ;; This is where wrappers are handled
  (if (null unique-iterators)
      (funcall f '())
    (iterate next-iterator ((itail unique-iterators)
                            (itable '()))
      (if (null itail)
          (funcall f (nreverse itable))
      (destructuring-bind (this-iterator . more-iterators) itail
        (multiple-value-bind (binding-sets valid-form cursor-form wrapper)
            (iterator-optimizers (unique-iterator-iterator this-iterator) environment)
          (funcall
           (or wrapper #'identity)
           (iterate next-binding-set ((bstail binding-sets))
             (if (null bstail)
                 (next-iterator more-iterators (acons this-iterator
                                                      (make-itable-entry
                                                       :valid-form valid-form
                                                       :cursor-form cursor-form)
                                                      itable))
               (destructuring-bind ((vars form &rest decls) . more-binding-sets) bstail
                 `(multiple-value-bind ,vars ,form
                    ,@decls
                    ,(next-binding-set more-binding-sets))))))))))))

(defmacro compiling-iterator-optimizers ((itable-var unique-iterators environment) &body forms)
  `(call/compiling-iterator-optimizers
    (lambda (,itable-var)
      ,@forms)
    ,unique-iterators
    ,environment))

(defun compile-bindings (body bindings itable)
  ;; Wrap forms corresponding BINDINGS around BODY, a list of forms.
  ;; ITABLE is an alist which maps from unique iterators to
  ;; itable-entries
  (if (null bindings)
      `(progn ,@body)
    (iterate next-binding ((btail bindings))
      (destructuring-bind (this-binding . more-bindings) btail
        (etypecase this-binding
          (let-binding
           (multiple-value-bind (vars/inits declarations)
               (with-collectors (var/init declaration)
                 (dolist (clause (let-binding-clauses this-binding))
                   (let ((cursor-form (let ((ie (cdr (assoc (clause-unique-iterator clause)
                                                            itable))))
                                        (unless ie
                                          (catastrophe "no cursor form"))
                                        (ie-cursor-form ie))))
                     (destructuring-match (clause-vars clause)
                       ((var)
                        (let ((variable (if (var-anonymous-p var)
                                            (make-symbol (symbol-name (var-name var)))
                                          (var-name var))))
                          (var/init `(,variable ,cursor-form))
                          (if (var-anonymous-p var)
                              (declaration `(declare (ignore ,variable)))
                            (progn
                              (unless (eq (var-type var) t)
                                (declaration `(declare (type ,(var-type var) ,variable))))
                              (when (var-special-p var)
                                (declaration `(declare (special ,variable))))
                              (when (var-ignore-p var)
                                (declaration `(declare (ignore ,variable))))
                              (when (var-ignorable-p var)
                                (declaration `(declare (ignorable ,variable))))
                              (when (not (null (var-declarations var)))
                                (declaration `(declare ,@(mapcar (lambda (declaration)
                                                                   `(,declaration ,variable))
                                                                 (var-declarations var)))))))))
                       (otherwise
                        (catastrophe "multiple varables in a let binding's clause"))))))
             `(let ,vars/inits
                ,@declarations
                ,@(if (null more-bindings)
                      body
                    `(,(next-binding more-bindings))))))
          (multiple-value-binding
           (let* ((clause (multiple-value-binding-clause this-binding))
                  (cursor-form (let ((ie (cdr (assoc (clause-unique-iterator clause)
                                                     itable))))
                                 (unless ie
                                   (catastrophe "no cursor form"))
                                 (ie-cursor-form ie))))
             (multiple-value-bind (variables declarations)
                 (with-collectors (variable declaration)
                   (dolist (var (clause-vars clause))
                     (let ((variable (if (var-anonymous-p var)
                                         (make-symbol (symbol-name (var-name var)))
                                       (var-name var))))
                       (variable variable)
                       (if (var-anonymous-p var)
                           (declaration `(declare (ignore ,variable)))
                         (progn
                           (unless (eq (var-type var) t)
                             (declaration `(declare (type ,(var-type var) ,variable))))
                           (when (var-special-p var)
                             (declaration `(declare (special ,variable)))))))))
               `(multiple-value-bind ,variables ,cursor-form
                  ,@declarations
                  ,@(if (null more-bindings)
                        body
                      `(,(next-binding more-bindings))))))))))))

(defmacro with-blocks (names &body forms)
  ;; Wrap a bunch of named blocks around something
  (cond
   ((null names)
    `(progn ,@forms))
   ((null (rest names))
    `(block ,(first names) ,@forms))
   (t
    (let* ((name (first names))
           (more-names (remove name (rest names))))
      (if (null more-names)
          `(block ,name ,@forms)
        `(block ,name
           (with-blocks ,more-names ,@forms)))))))

(defun expand-for (clauses body environment &key (top t) (name 'for))
  (multiple-value-bind (ok clauses) (parse-clause-descriptions clauses)
    (unless ok
      (syntax-error clauses "bad clause or clauses"))
    (with-names (<start> <end>)
      (let ((bindings (clauses->bindings clauses)))
        (compiling-iterator-optimizers (itable (mapcar #'clause-unique-iterator clauses)
                                               environment)
          (let ((compiled-bindings (compile-bindings body bindings itable)))
            (if top
                `(with-blocks (,name nil)
                   (flet ((final (&rest values)
                            (declare (dynamic-extent values))
                            (return (values-list values)))
                          (final* (&rest values)
                            (declare (dynamic-extent values))
                            (return-from ,name (values-list values))))
                     (declare (inline final final*)
                              (ignorable (function final) (function final*)))
                     (tagbody
                      ,<start>
                      (unless ,(if (= (length itable) 1)
                                   (ie-valid-form (cdr (first itable)))
                                 `(and ,@(mapcar (lambda (ie)
                                                   (ie-valid-form (cdr ie)))
                                                 itable)))
                        ;; some valid's aren't: we're done
                        (go ,<end>))
                      (flet ((next () (go ,<start>))
                             (next* () (go ,<start>)))
                        (declare (inline next next*)
                                 (ignorable (function next) (function next*)))
                        ,compiled-bindings)
                      (go ,<start>)
                      ,<end>
                      nil)))
              `(with-blocks (nil)
                 (flet ((final (&rest values)
                          (declare (dynamic-extent values))
                          (return (values-list values))))
                   (declare (inline final)
                            (ignorable (function final)))
                   (tagbody
                    ,<start>
                    (unless ,(if (= (length itable) 1)
                                 (ie-valid-form (cdr (first itable)))
                               `(and ,@(mapcar (lambda (ie)
                                                 (ie-valid-form (cdr ie)))
                                               itable)))
                      ;; some valid's aren't: we're done
                      (go ,<end>))
                    (flet ((next () (go ,<start>)))
                      (declare (inline next)
                               (ignorable (function next)))
                      ,compiled-bindings)
                    (go ,<start>)
                    ,<end>
                    nil))))))))))

(defmacro for (clauses &body body &environment environment)
  "Iteration construct

See the manual."
  (expand-for clauses body environment))

(defmacro for* (clauses &body body &environment environment)
  "Nested iteration construct

See the manual."
  (if (null clauses)
      (expand-for clauses body environment :top t :name 'for*)
    (iterate expand-for* ((ctail clauses)
                          (top t))
      (destructuring-bind (this-clause . more-clauses) ctail
        (if (null more-clauses)
            (expand-for (list this-clause) body environment :top top :name 'for*)
          (expand-for (list this-clause)
                      `(,(expand-for* more-clauses nil))
                      environment
                      :top top :name 'for*))))))
