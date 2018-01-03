(defparameter dt 0.01d0)
(defparameter n 2000000)
(defparameter a 10.0d0)
(defparameter b 28.0d0)
(defparameter c (/ 8.0d0 3))

(defvar x (make-array (list (+ n 1)) :initial-element 0.0d0))
(defvar y (make-array (list (+ n 1)) :initial-element 0.0d0))
(defvar z (make-array (list (+ n 1)) :initial-element 0.0d0))


(setf (aref x 1) 0.0d0)
(setf (aref y 1) 1.0d0)
(setf (aref z 1) 1.05d0)

(time
 (loop for i from 1 to (1- n)
    do 
      (setf (aref x (1+ i))
            (+ (aref x i)
               (* dt (+ (- (* a (aref x i))) (* a (aref y i))))))
      (setf (aref y (1+ i))
            (+ (aref y i)
               (* dt (+ (- (* (aref x i) (aref z i))) (* b (aref x i))  (- (aref y i))))))
      (setf (aref z (1+ i))
            (+ (aref z i)
               (* dt (- (* (aref x i) (aref y i)) (* c (aref z i))))))))


