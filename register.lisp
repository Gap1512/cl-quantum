;;;; register.lisp
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(in-package #:cl-quantum)

(defclass/std register ()
  ((n-qubits :std 0 :type integer)
   (state :type (simple-array (complex single-float) *))))

(defun make-register (n-qubits)
  (make-instance 'register :n-qubits n-qubits))

(defmethod initialize-instance :after ((register register) &key)
  (let ((new-state (make-array (expt 2 (n-qubits register))
			       :initial-element (complex 0.0))))
    (setf (aref new-state 0) (complex 1.0)
	  (state register) new-state)))

(defmethod wave-function ((register register))
  (state register))


