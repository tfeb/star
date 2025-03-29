;;;; Loader for unroll
;;;

(in-package :asdf-user)

(load (merge-pathnames (make-pathname :directory '(:relative :up)
                                      :name "org.tfeb.star"
                                      :type "asd")
                       *load-truename*))

(load-system "org.tfeb.star/unroll")
