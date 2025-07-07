;;;; Test preamble
;;;

(in-package :org.tfeb.star/test)

(unless *test-individually*
  (let ((result (test "org.tfeb.star" :report *test-report-class*)))
    (when (eq (status result) ':failed)
      (error "Tests failed" result))
    result))
