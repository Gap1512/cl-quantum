;;;; functional-gates.lisp
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(in-package #:cl-quantum)

(defun p (phi)
  (list (list 1 0)
	(list 0 (exp (* +i+ phi)))))

(defun r-x (phi)
  (let ((phi/2 (/ phi 2)))
    (list (list (custom-cos phi/2) (* (- +i+) (custom-sin phi/2)))
	  (list (* (- +i+) (custom-sin phi/2)) (custom-cos phi/2)))))

(defun r-y (phi)
  (let ((phi/2 (/ phi 2)))
    (list (list (custom-cos phi/2) (- (custom-sin phi/2)))
	  (list (custom-sin phi/2) (custom-cos phi/2)))))

(defun r-z (phi)
  (let ((i*phi/2 (* +i+ (/ phi 2))))
    (list (list (exp (- i*phi/2)) 0)
	  (list 0 (exp i*phi/2)))))
