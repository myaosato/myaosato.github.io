(defconstant dt 0.01d0)
(defconstant n 2000000)
(defconstant a 10.0d0)
(defconstant b 28.0d0)
(defconstant c #.(/ 8.0d0 3)) 

(defun main ()
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let ((x (make-array n :element-type 'double-float))
	(y (make-array n :element-type 'double-float))
	(z (make-array n :element-type 'double-float)))
    (setf (aref x 0) 0.0d0
	  (aref y 0) 1.0d0
	  (aref z 0) 1.05d0)
    (dotimes (i #.(1- n))
      (declare (fixnum i))
      (setf (aref x (1+ i)) (+ (aref x i) (* #.dt (- (* #.a (aref y i)) (* #.a (aref x i)))))
	    (aref y (1+ i)) (+ (aref y i) (* #.dt (- (* #.b (aref x i)) (* (aref x i) (aref z i)) (aref y i))))
	    (aref z (1+ i)) (+ (aref z i) (* #.dt (- (* (aref x i) (aref y i)) (* #.c (aref z i)))))))
    ))

(time (main))
