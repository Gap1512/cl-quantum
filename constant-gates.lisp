;;;; constant-gates.lisp
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(in-package #:cl-quantum)

(alexandria:define-constant +ket-0+ :0)

(alexandria:define-constant +ket-1+ :1)

(alexandria:define-constant +zero+ (list (list 0 0) (list 0 0)) :test #'equal)

(alexandria:define-constant +identity+ (list (list 1 0) (list 0 1)) :test #'equal)

(alexandria:define-constant +pauli-x+ (list (list 0 1) (list 1 0)) :test #'equal)

(alexandria:define-constant +pauli-y+ (list (list 0 (- +i+)) (list +i+ 0)) :test #'equal)

(alexandria:define-constant +pauli-z+ (list (list 1 0) (list 0 -1)) :test #'equal)

(alexandria:define-constant +ket-0-bra-0+ (list (list 1 0) (list 0 0)) :test #'equal)

(alexandria:define-constant +ket-0-bra-1+ (list (list 0 1) (list 0 0)) :test #'equal)

(alexandria:define-constant +ket-1-bra-0+ (list (list 0 0) (list 1 0)) :test #'equal)

(alexandria:define-constant +ket-1-bra-1+ (list (list 0 0) (list 0 1)) :test #'equal)

(alexandria:define-constant +sqrt-not+ (scalar 0.5 (list (list (+ 1 +i+) (- 1 +i+))
							 (list (- 1 +i+) (+ 1 +i+))))
  :test #'equal)

(alexandria:define-constant +hadamard+ (scalar (/ (sqrt 2)) (list (list 1 1)
								  (list 1 (- 1))))
  :test #'equal)

(alexandria:define-constant +s+ (p (/ pi 2)) :test #'equal)

(alexandria:define-constant +s-t+ (p (- (/ pi 2))) :test #'equal)

(alexandria:define-constant +t+ (p (/ pi 4)) :test #'equal)

(alexandria:define-constant +t-t+ (p (- (/ pi 4))) :test #'equal)
