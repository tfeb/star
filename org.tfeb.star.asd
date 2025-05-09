;;;; ASDF sysdcl for Štar
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.star"
  :description "Štar: an iteration construct"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/star"
  :depends-on ("org.tfeb.conduit-packages/define-package"
               "org.tfeb.hax.utilities"
               "org.tfeb.hax.process-declarations"
               "org.tfeb.hax.let-values"
               "org.tfeb.dsm"
               "org.tfeb.hax.collecting"
               "org.tfeb.hax.iterate")
  :in-order-to ((test-op (load-op "org.tfeb.star/test")))
  :serial t
  :components
  ((:file "pkg")
   (:file "common")
   (:file "star")
   (:file "iterators")))

(defsystem "org.tfeb.star/test"
  :description "Štar tests"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/star"
  :depends-on ("org.tfeb.hax.collecting"
               "org.tfeb.hax.iterate"
               "org.tfeb.hax.utilities"
               "org.tfeb.dsm"
               "org.tfeb.star"
               "parachute")
  :pathname "test/"
  :serial t
  :components
  ((:file "pkg")
   (:file "preamble")
   (:file "test-sanity")
   (:file "test-iterators")
   #+(or LispWorks SBCL)
   (:file "test-declarations")
   (:file "test-all")))

(defsystem "org.tfeb.star/bench"
  :description "Štar benchmarks"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/star"
  :depends-on ("org.tfeb.hax.collecting"
               "org.tfeb.hax.iterate"
               "org.tfeb.star")
  :pathname "bench/"
  :serial t
  :components
  ((:file "pkg")
   (:file "preamble")
   (:file "simple")
   (:file "unrolled")
   (:file "collecting")))

(defsystem "org.tfeb.star/unroll"
  :description "Experimental unrolling support for Štar"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/star"
  :depends-on ("org.tfeb.star"
               "org.tfeb.conduit-packages/define-package"
               "org.tfeb.hax.utilities"
               "org.tfeb.hax.let-values"
               "org.tfeb.dsm"
               "org.tfeb.hax.collecting"
               "org.tfeb.hax.iterate")
  :pathname "unroll/"
  :serial t
  :components
  ((:file "pkg")
   (:file "unroll")
   (:file "rolled")))
