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
;;; - additional information, which may be:
;;;   - a designator for a wrapper function which is which is called with the entire form
;;;   - a plist with keys :WRAPPER and :TYPES and possibly other keys
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

(defvar *obey-iterator-optimizer-types* t
  "If true, type information from iterator optimizers will be interpolated")

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
  (let-values (((name table)
                (destructuring-match name/table
                  ((name table)
                   (:when (symbolp name))
                   (values name table))
                  (name
                   (:when (symbolp name))
                   (values name '(car *iterator-optimizers*)))
                  (otherwise
                   (star-syntax-error name/table "bad name / table"))))
               ((decls body) (parse-simple-body forms)))
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
       ',name)))

(defun iterator-optimizers (iterator environment)
  ;; Return found values which explain how to optimize an iterator: a
  ;; list of binding sets, a valid form a cursor form and additional
  ;; information.  In the case where there is no optimizer return
  ;; built-in versions
  (flet ((fallback-optimizers (valid-name cursor-name
                               &optional note &rest reasons)
           ;; Note this only returns three forms, because it's the
           ;; fallback: it can't decline to handle it.
           (when note
             (apply #'star-note note reasons))
           (values
            ;; binding set
            `(((,valid-name ,cursor-name)
               ,iterator
               (declare (type function ,valid-name ,cursor-name))))
            ;; form returning true if more
            `(funcall ,valid-name)
            ;; form returning values
            `(funcall ,cursor-name)
            ;; no additional
            nil)))
    (destructuring-match iterator
                         ((f &rest _)
                          (:when (symbolp f))
                          (if *enable-iterator-optimizers*
                              (multiple-value-bind (found stack) (find-iterator-optimizer f)
                                (if found
                                    (multiple-value-bind
                                        (handled binding-sets valid cursor additional)
                                        (funcall found iterator environment stack)
                                      (if handled
                                          (values binding-sets valid cursor additional)
                                        (fallback-optimizers
                                         (make-symbol (format nil "~A-VALID" (symbol-name f)))
                                         (make-symbol (format nil "~A-CURSOR" (symbol-name f)))
                                         "optimizer for ~S rejected ~S" f iterator)))
                                  (fallback-optimizers
                                   (make-symbol (format nil "~A-VALID" (symbol-name f)))
                                   (make-symbol (format nil "~A-CURSOR" (symbol-name f)))
                                   "no optimizer for ~S" f)))
                              (fallback-optimizers
                               (make-symbol (format nil "~A-VALID" (symbol-name f)))
                               (make-symbol (format nil "~A-CURSOR" (symbol-name f))))))
                         (otherwise
                          (fallback-optimizers (make-symbol "VALID") (make-symbol "CURSOR"))))))

;;;; Štar itself
;;;

;;; Handling declarations
;;;

(defun declarations-for-variables (declarations variables &key (complement nil) (environment nil))
  ;; Return a list of declarations which apply to VARIABLES.
  ;; COMPLEMENT means return a declaration whose specifiers are all
  ;; those which *don't* apply to VARIABLES.  ENVIRONMENT is used for
  ;; compile-time type information
  (let* ((atomic-specifiers
          (collecting
            (dolist (declaration declarations)
              (dolist (specifier (rest declaration))
                (processing-declaration-specifier (specifier
                                                   :constructor maker
                                                   :bindings (variable-names function-names
                                                                             specifier)
                                                   :environment environment)
                  (when variable-names
                    (dolist (v variable-names)
                      (collect (maker :variable-names (list v)))))
                  (when function-names
                    (dolist (f function-names)
                      (collect (maker :function-names (list f)))))
                  (when (not (or variable-names function-names))
                    (collect specifier)))))))
         (selected-specifiers
          (collecting
            (dolist (spec atomic-specifiers)
              (processing-declaration-specifier (spec
                                                 :bindings (variable-names specifier)
                                                 :environment environment)
                (if (not (null variable-names))
                    (destructuring-bind (var) variable-names
                      (if (not complement)
                          (when (member var variables)
                            (collect specifier))
                        (unless (member var variables)
                          (collect specifier))))
                  (when complement
                    (collect specifier))))))))
    (if (not (null selected-specifiers))
        `((declare ,@selected-specifiers))
      ())))

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
  ;; a clause contains its var objects, a list of interesting
  ;; (non-anonymous) variable names, to which declarations may apply,
  ;; and its (unique) iterator
  (vars (catastrophe "no vars"))
  (interesting-varnames '())
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
                        :ignore-p ignore ;
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
       (let ((anonymous (anonymous-variable-p variable)))
         (make-clause :vars (list (make-var :name variable :anonymous-p anonymous))
                      :interesting-varnames (if anonymous '() (list variable))
                      :unique-iterator (unique-iterator iterator))))
      (((variable &rest args &key name type anonymous special ignore ignorable declarations)
        iterator)
       (:when (symbolp variable))
       (declare (ignore name type anonymous special ignore ignorable declarations))
       (let ((var (parse-complex-variable variable args)))
         (make-clause :vars (list var)
                      :interesting-varnames (if (var-anonymous-p var) '() (list variable))
                      :unique-iterator (unique-iterator iterator))))
      (((&rest vardescs) iterator)
       (multiple-value-bind (vars interesting-varnames)
           (with-collectors (var interesting-name)
             (dolist (vardesc vardescs)
               (destructuring-match vardesc
                 (variable
                  (:when (symbolp variable))
                  (let ((anonymous (anonymous-variable-p variable)))
                    (var (make-var :name variable :anonymous-p anonymous))
                    (unless anonymous
                      (interesting-name variable))))
                 ((variable &rest args &key &allow-other-keys)
                  (:when (symbolp variable))
                  (let ((var (parse-complex-variable variable args)))
                    (var var)
                    (unless (var-anonymous-p var)
                      (interesting-name variable))))
                 (otherwise
                  (return-from parse-clause-description nil)))))
       (make-clause :vars vars
                    :interesting-varnames interesting-varnames
                    :unique-iterator (unique-iterator iterator))))
      (otherwise nil))))

(defun parse-clause-descriptions (clause-descriptions)
  ;; return either T and a list of clauses, or NIL and NIL if things
  ;; are hopeless, or NIL and a list of offending clause descriptions,
  ;; if things are less bad.
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
  cursor-form
  types)

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
          (multiple-value-bind (binding-sets valid-form cursor-form additional)
              (iterator-optimizers (unique-iterator-iterator this-iterator) environment)
            (multiple-value-bind (wrapper types)
                (destructuring-match additional
                  (f
                   (:when (functionp f))
                   (values f nil))
                  (f
                   (:when (and (symbolp f)
                               (fboundp f)))
                   (values (symbol-function f) nil))
                  ((&key (wrapper #'identity) (types nil) &allow-other-keys)
                   (values wrapper types))
                  (otherwise
                   (star-error "optimizer additional isn't")))
              (funcall
               wrapper
               (iterate next-binding-set ((bstail binding-sets))
                 (if (null bstail)
                     (next-iterator more-iterators (acons this-iterator
                                                          (make-itable-entry
                                                           :valid-form valid-form
                                                           :cursor-form cursor-form
                                                           :types types)
                                                          itable))
                   (destructuring-bind ((vars form &rest decls) . more-binding-sets) bstail
                     `(multiple-value-bind ,vars ,form
                        ,@decls
                        ,(next-binding-set more-binding-sets)))))))))))))

(defmacro compiling-iterator-optimizers ((itable-var unique-iterators environment) &body forms)
  `(call/compiling-iterator-optimizers
    (lambda (,itable-var)
      ,@forms)
    ,unique-iterators
    ,environment))

(defun compile-bindings (bindings declarations forms itable)
  ;; Wrap forms corresponding to BINDINGS around DECLARATIONS and FORMS, a
  ;; list of declarations and forms.  ITABLE is an alist which maps
  ;; from unique iterators to itable-entries
  (if (null bindings)
      `(locally ,@declarations ,@forms)
    (iterate next-binding ((btail bindings))
      (destructuring-bind (this-binding . more-bindings) btail
        (etypecase this-binding
          (let-binding
           (multiple-value-bind (vars/inits our-declarations)
               (with-collectors (var/init declaration)
                 (dolist (clause (let-binding-clauses this-binding))
                   (multiple-value-bind (cursor-form iterator-types)
                       (let ((ie (cdr (assoc (clause-unique-iterator clause)
                                             itable))))
                         (unless ie (catastrophe "no itable entry"))
                         (values (ie-cursor-form ie)
                                 (ie-types ie)))
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
                                                                 (var-declarations var)))))
                              (destructuring-match iterator-types
                                ((type &rest _)
                                 (:when type)
                                 (if *obey-iterator-optimizer-types*
                                     (progn
                                       (star-note "implicitly declaring ~S as ~S"
                                                  variable type)
                                       (declaration `(declare (type ,type ,variable))))
                                   (star-note "not implicitly declaring ~S as ~S"
                                              variable type))))))))
                       (otherwise
                        (catastrophe "multiple varables in a let binding's clause"))))))
             `(let ,vars/inits
                ,@our-declarations
                ,@declarations
                ,@(if (null more-bindings)
                      forms
                    `(,(next-binding more-bindings))))))
          (multiple-value-binding
           (let*-values (((clause) (multiple-value-binding-clause this-binding))
                         ((cursor-form iterator-types)
                          (let ((ie (cdr (assoc (clause-unique-iterator clause)
                                                itable))))
                            (unless ie (catastrophe "no itable entry"))
                            (values (ie-cursor-form ie) (ie-types ie)))))
             (unless (typep iterator-types 'list)
               (star-error "types from iterator optimizer isn't a list?"))
             (multiple-value-bind (variables our-declarations)
                 (with-collectors (variable declaration)
                   ;; Iterate over the variables and any iterator
                   ;; types for them.
                   (do* ((vt (clause-vars clause) (rest vt))
                         (var (first vt) (first vt))
                         (tt iterator-types (rest tt))
                         (tp (first tt) (first tt)))
                        ((null vt))
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
                             (declaration `(declare (special ,variable))))
                           (when tp
                             (if *obey-iterator-optimizer-types*
                                 (progn
                                   (star-note "implicitly declaring ~S as ~S"
                                              variable tp)
                                   (declaration `(declare (type ,tp ,variable))))
                               (star-note "not implicitly declaring ~S as ~S"
                                          variable tp))))))))
               `(multiple-value-bind ,variables ,cursor-form
                  ,@our-declarations
                  ,@declarations
                  ,@(if (null more-bindings)
                        forms
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

(defun expand-for (clauses body environment &key (for* nil) (name (if for* 'for* 'for)))
  ;; Expand FOR and FOR*.  This now has the recursive expansion,
  ;; formerly done at the macro level, for FOR* in its being, which
  ;; lets declaration-raising work
  (let-values (((ok clauses) (parse-clause-descriptions clauses))
               ((declarations forms) (parse-simple-body body)))
    (unless ok
      (star-syntax-error clauses "bad clause or clauses"))
    (iterate expand ((current (if for* (list (first clauses)) clauses))
                     (more (if for* (rest clauses) '()))
                     (top t)
                     (seen-varnames '()))
      (with-names (<start> <end>)
        (let* ((bindings (clauses->bindings current))
               (our-varnames (collecting
                               (dolist (clause current)
                                 (dolist (name (clause-interesting-varnames clause))
                                   (collect name)))))
               (effective-declarations
                (if (null more)
                    (append
                     (declarations-for-variables declarations our-varnames
                                                 :environment environment)
                     (declarations-for-variables declarations
                                                 (append our-varnames seen-varnames)
                                                 :environment environment
                                                 :complement t))
                  (declarations-for-variables declarations our-varnames
                                              :environment environment)))
               (effective-forms (if (null more)
                                    forms
                                  (list
                                   (expand (list (first more))
                                           (rest more)
                                           nil
                                           (append our-varnames seen-varnames))))))
          (compiling-iterator-optimizers (itable (mapcar #'clause-unique-iterator current)
                                                 environment)
            (let ((compiled-bindings (compile-bindings bindings
                                                       effective-declarations
                                                       effective-forms
                                                       itable)))
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
                                     (ie-valid-form (cdr (first itable))) ;
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
                      nil)))))))))))

(defmacro for ((&rest clauses) &body body &environment environment)
  "Iteration construct

See the manual."
  (expand-for clauses body environment))

(defmacro for* ((&rest clauses) &body body &environment environment)
  "Nested iteration construct

See the manual."
  (expand-for clauses body environment :for* t))
