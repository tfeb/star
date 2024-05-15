;;;; Packages for Štar
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.conduit-packages/define-package
   :org.tfeb.hax.utilities
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  :compile t))

(org.tfeb.conduit-packages/define-package:define-package :org.tfeb.star/pkg
  (:use :cl :org.tfeb.conduit-packages/define-package))

(in-package :org.tfeb.star/pkg)

(define-package :org.tfeb.star/common
  ;; Common things between impl and iterators, conditions &c
  (:use :cl)
  (:use :org.tfeb.hax.utilities)
  (:export
   #:*builtin-iterator-optimizer-table*
   #:*star-bootstrap*
   #:star-error
   #:syntax-error
   #:catastrophe
   #:star-note
   #:reporting-star-notes
   #:note))

(define-package :org.tfeb.star/impl
  ;; Štar's implementation
  (:use :cl)
  (:use
   :org.tfeb.hax.utilities
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:use
   :org.tfeb.star/common)
  (:export
   ;; Common things
   #:star-error
   #:star-note
   #:reporting-star-notes)
  (:export
   ;; Iterator protocol
   #:make-iterator-optimizer-table
   #:*iterator-optimizers*
   #:*enable-iterator-optimizers*
   #:define-iterator-optimizer
   #:get-iterator-optimizer
   #:map-iterator-optimizer-table
   #:remove-iterator-optimizer
   #:find-iterator-optimizer)
  (:export
   ;; Štar
   #:for
   #:for*
   #:next
   #:next*
   #:final
   #:final*))

(define-package :org.tfeb.star/iterators
  ;; Iterators
  (:use :cl)
  (:use
   :org.tfeb.star/impl
   :org.tfeb.hax.utilities
   :org.tfeb.dsm
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:use
   :org.tfeb.star/common)
  (:export
   ;; Builtin iterators
   #:in-sequence
   #:in-list
   #:on-list
   #:in-vector
   #:in-hash-table
   #:stepping
   #:sequentially-calling
   #:sequentially
   #:sequentially-calling*
   #:sequentially*
   #:cyclically-calling
   #:cyclically
   #:in-iterators
   #:in-parallel-iterators
   #:always
   #:in-range))

(define-package :org.tfeb.star
  ;; The whole thing
  (:use)
  (:extends
   :org.tfeb.star/impl
   :org.tfeb.star/iterators))

(define-package :org.tfeb.*
  ;; Pure
  (:use)
  (:extends
   :org.tfeb.star/impl))
