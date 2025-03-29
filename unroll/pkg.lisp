;;;; Packages for Štar unroll support
;;;
;;; Packages with docstrings are public
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.conduit-packages/define-package
   :org.tfeb.star
   :org.tfeb.hax.utilities
   :org.tfeb.dsm
   :org.tfeb.hax.let-values
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  :compile t))

(org.tfeb.conduit-packages/define-package:define-package :org.tfeb.star/unroll/pkg
  (:use :cl :org.tfeb.conduit-packages/define-package))

(in-package :org.tfeb.star/unroll/pkg)

(define-conduit-package :org.tfeb.star/without-for
  (:extends/excluding :org.tfeb.star #:for)
  (:export #:for))

(define-package :org.tfeb.star/unroll/impl
  (:use :cl)
  (:use
   :org.tfeb.star/without-for
   :org.tfeb.hax.utilities
   :org.tfeb.dsm
   :org.tfeb.hax.let-values
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:export
   ;; Iterator unroller protocol
   #:make-iterator-unroller-table
   #:*iterator-unrollers*
   #:define-iterator-unroller
   #:get-iterator-unroller
   #:map-iterator-unroller-table
   #:remove-iterator-unroller
   #:find-iterator-unroller)
  (:export
   ;; Unrolling, and control
   #:rolled
   #:for
   #:for/unroll
   #:*enable-unrolling*
   #:*unroll-by*))

(define-conduit-package :org.tfeb.star/unroll
  (:extends :org.tfeb.star/without-for)
  (:extends :org.tfeb.star/unroll/impl))
