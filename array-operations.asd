(defpackage #:array-operations-asd
  (:use :cl :asdf))

(in-package :array-operations-asd)

(defsystem array-operations
  :description "Array operations (formerly part of FFA)"
  :author "Tamas K Papp"
  :license "GPL"
  :serial t
  :components ((:file "package")
	       (:file "displaced-utils")
	       (:file "operations"))
  :depends-on (:cffi :cl-utilities :metabang-bind :iterate))
