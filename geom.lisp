;;;; Poincare ball, curvature -1. No optional args. No macros.

(defconstant +ham-eps+ 1.0d-6)
(defconstant +ham-step+ 0.55d0)

(defun ham-acosh (x)
  (log (+ x (sqrt (- (* x x) 1.0d0)))))

(defun ham-tanh (x)
  (let ((e (exp (* 2.0d0 x))))
    (/ (- e 1.0d0) (+ e 1.0d0))))

(defun ham-dot (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b))))

(defun ham-norm (a)
  (sqrt (ham-dot a a)))

(defun ham-clamp (a)
  (let ((n (ham-norm a))
        (limit (- 1.0d0 +ham-eps+)))
    (if (< n limit)
        a
        (list (* (car a) (/ limit n)) (* (cadr a) (/ limit n))))))

(defun ham-mobius (x y)
  (let* ((x2 (ham-dot x x))
         (y2 (ham-dot y y))
         (xy (ham-dot x y))
         (den (+ 1.0d0 (* 2.0d0 xy) (* x2 y2)))
         (k1 (+ 1.0d0 (* 2.0d0 xy) y2))
         (k2 (- 1.0d0 x2)))
    (ham-clamp (list (/ (+ (* k1 (car x)) (* k2 (car y))) den)
                     (/ (+ (* k1 (cadr x)) (* k2 (cadr y))) den)))))

(defun ham-dist (x y)
  (let* ((x2 (min (ham-dot x x) (- 1.0d0 +ham-eps+)))
         (y2 (min (ham-dot y y) (- 1.0d0 +ham-eps+)))
         (dx (- (car x) (car y)))
         (dy (- (cadr x) (cadr y)))
         (arg (+ 1.0d0 (/ (* 2.0d0 (+ (* dx dx) (* dy dy)))
                          (* (- 1.0d0 x2) (- 1.0d0 y2))))))
    (ham-acosh (max arg 1.0d0))))

(defun ham-place (parent sibling depth)
  (let* ((radius (ham-tanh (/ (* depth +ham-step+) 2.0d0)))
         (pn (ham-norm parent))
         (base (if (< pn +ham-eps+) 0.0d0 (atan (cadr parent) (car parent))))
         (angle (+ base (* sibling (/ pi 6.0d0)))))
    (ham-clamp (list (* radius (cos angle)) (* radius (sin angle))))))
