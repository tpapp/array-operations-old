(defpackage #:array-operations-asd
  (:use :cl :asdf))

(in-package :array-operations-asd)

(defsystem array-operations
  :description "Array operations (formerly part of FFA)"
  :author "Tamas K Papp"
  :license "GPL"
  :serial t
  :components ((:file "package")
	       (:file "ffa" :depends-on ("package"))
	       (:file "displaced-utils" :depends-on ("ffa"))
	       (:file "operations" :depends-on ("displaced-utils")))
  :depends-on (:cffi :cl-utilities :metabang-bind :iterate))
