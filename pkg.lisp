;;;; Packages for Štar
;;;
;;; Packages with docstrings are public
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.conduit-packages/define-package
   :org.tfeb.hax.utilities
   :org.tfeb.hax.process-declarations
   :org.tfeb.hax.let-values
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  :compile t))

(org.tfeb.conduit-packages/define-package:define-package :org.tfeb.star/pkg
  (:use :cl :org.tfeb.conduit-packages/define-package))

(in-package :org.tfeb.star/pkg)

(define-package :org.tfeb.star/utilities
  (:Documentation "Utilities for Štar")
  (:use)
  (:export
   #:star-error
   #:star-syntax-error
   #:star-syntax-error-form
   #:star-note
   #:reporting-star-notes
   #:anonymous-variable-p))

(define-package :org.tfeb.star/iterator-optimizer-protocol
  (:documentation "Štar iterator optimizer protocol")
  (:nicknames :org.tfeb.star/iop)
  (:use)
  (:export
   ;; Iterator protocol
   #:make-iterator-optimizer-table
   #:*iterator-optimizers*
   #:*enable-iterator-optimizers*
   #:*obey-iterator-optimizer-types*
   #:define-iterator-optimizer
   #:get-iterator-optimizer
   #:map-iterator-optimizer-table
   #:remove-iterator-optimizer
   #:find-iterator-optimizer))

(define-package :org.tfeb.star/common
  ;; Common things between impl and iterators, conditions &c
  (:use :cl)
  (:use :org.tfeb.hax.utilities)
  (:use :org.tfeb.star/utilities)
  (:export
   #:*builtin-iterator-optimizer-table*
   #:*star-bootstrap*
   #:catastrophe))

(define-package :org.tfeb.star/impl
  ;; Štar's implementation
  (:use :cl)
  (:use
   :org.tfeb.hax.utilities
   :org.tfeb.hax.process-declarations
   :org.tfeb.hax.let-values
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:use
   :org.tfeb.star/utilities
   :org.tfeb.star/common
   :org.tfeb.star/iop)
  (:export
   ;; Štar.  FINAL and FINAL* should not exist.
   #:for
   #:for*
   #:next
   #:next*
   #:final
   #:final*))

(define-package :org.tfeb.star/iterators
  (:documentation "Štar predefined iterators")
  (:use :cl)
  (:use
   :org.tfeb.hax.utilities
   :org.tfeb.hax.let-values
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:use
   :org.tfeb.star/utilities
   :org.tfeb.star/common
   :org.tfeb.star/iop
   :org.tfeb.star/impl)
  (:export
   ;; Builtin iterators
   #:in-naturals
   #:in-sequence
   #:in-list
   #:on-list
   #:in-vector
   #:in-hash-table
   #:in-package-symbols
   #:stepping
   #:stepping*
   #:stepping-values
   #:sequentially-calling
   #:sequentially
   #:sequentially-calling*
   #:sequentially*
   #:cyclically-calling
   #:cyclically
   #:in-iterators
   #:in-parallel-iterators
   #:always))

(define-conduit-package :org.tfeb.star
  (:documentation "Štar, including iterators and iterator protocol")
  (:extends
   :org.tfeb.star/utilities
   :org.tfeb.star/impl
   :org.tfeb.star/iop
   :org.tfeb.star/iterators))

(define-conduit-package :org.tfeb.*
  (:documentation "Štar: just itself")
  (:extends
   :org.tfeb.star/impl))
