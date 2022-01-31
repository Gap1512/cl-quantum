;;;; cl-quantum.lisp
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(in-package #:cl-quantum)

(defclass/std qubit ()
  ((a :std 1.0)
   (b :std #C(0.0 0.0))))

(defmethod print-object ((qubit qubit) stream)
  (print-unreadable-object (qubit stream :type t :identity t)
    (format stream "[~{[~a]~^, ~}]" (wave-function qubit))))

(defmethod wave-function ((qubit qubit))
  (with-slots (a b) qubit
    (list a b)))

(defmethod theta ((qubit qubit))
  (* 2 (acos (a qubit))))

(defmethod phi ((qubit qubit))
  (with-slots (b) qubit
    (atan (imagpart b) (realpart b))))

(defmethod set-bloch-sphere ((qubit qubit) theta phi)
  (let ((normalized-theta (if (<= .0 theta pi)
			      theta
			      (- pi (mod theta pi))))
	(normalized-phi (if (<= (- pi) phi pi)
			phi
			(- (mod phi (* 2 pi)) (* 2 pi)))))
    (setf (a qubit) (custom-cos (/ normalized-theta 2))
	  (b qubit) (* (custom-sin (/ normalized-theta 2))
		       (exp (* +i+ normalized-phi))))))

(defmethod set-probability-amplitude ((qubit qubit) a b)
  (let* ((quadratic-norm-a (quadratic-norm a))
	 (quadratic-norm-b (quadratic-norm b))
	 (probability (+ quadratic-norm-a quadratic-norm-b))
	 (new-a (if (practical-zerop probability)
		    0
		    (sqrt (/ quadratic-norm-a probability))))
	 (abs-b (abs b)))
    (setf (a qubit) new-a
	  (b qubit) (if (practical-zerop new-a)
			1
			(if (practical-zerop abs-b)
			    0
			    (* (sqrt (/ quadratic-norm-b probability))
			       (/ b abs-b)
			       (/ (abs new-a) new-a)))))))

(defun make-qubit (a b)
  (make-instance 'qubit :a a :b b))

(defmethod initialize-instance :after ((qubit qubit) &key)
  (set-probability-amplitude qubit (a qubit) (b qubit)))

(defmethod set-wave-function ((qubit qubit) wave-function)
  (set-probability-amplitude qubit
			     (first wave-function)
			     (second wave-function)))

(defmethod measure ((qubit qubit))
  (if (<= (random 1.0) (quadratic-norm (a qubit)))
      +ket-0+
      +ket-1+))

(defmethod simulate ((qubit qubit) &optional (times 1000))
  (let ((measures (loop repeat times collecting (measure qubit))))
    (format t "0: ~a%~%1: ~a%~%"
	    (* 100 (float (/ (count +ket-0+ measures) times)))
	    (* 100 (float (/ (count +ket-1+ measures) times))))))

(defun apply-gate (gate &rest qubits)
  (case (length qubits)
    (0 (error "You need to pass at least one qubit"))
    (1 (let ((new-qubit (make-instance 'qubit)))
	 (set-wave-function new-qubit (dot (wave-function (car qubits)) gate))
	 new-qubit))))
