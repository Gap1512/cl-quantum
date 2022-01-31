;;;; cl-quantum.asd
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(asdf:defsystem #:cl-quantum
  :description "Quantum simulator for Common Lisp"
  :author "Gustavo Alves Pacheco"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:defclass-std #:alexandria)
  :components ((:file "package")
	       (:file "math")
	       (:file "functional-gates")
	       (:file "constant-gates")
               (:file "cl-quantum")))
