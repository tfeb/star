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
               "org.tfeb.dsm"
               "org.tfeb.hax.collecting"
               "org.tfeb.hax.iterate")
  :in-order-to ((test-op (load-op "org.tfeb.star/test")))
  :serial t
  :components
  ((:file "pkg")
   (:file "common")
   (:file "star")
   (:file "iterators")
   (:file "ranges")))

(defsystem "org.tfeb.star/test"
  :description "Štar tests"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/star"
  :depends-on ("org.tfeb.star" "parachute")
  :pathname "test/"
  :serial t
  :components
  ((:file "pkg")))

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
   (:file "simple")))
