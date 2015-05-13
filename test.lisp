(print (equal-tensor (s 4) (.+ (s 1) (s 3))))
(print (equal-tensor (s 2) (./ (s 6) (s 3))))
(print (equal-tensor (s (/ 5 2)) (./ (s 5) (s 2))))
(print (equal-tensor (s 2.5) (./ (s 5.0) (s 2))))
(print (equal-tensor (s 3) (.- (s 5) (s 2))))
(print (equal-tensor (s 10) (.* (s 5) (s 2))))

(print (equal-tensor (v 5 7 9) (.+ (v 1 2 3) (v 4 5 6))))
(print (equal-tensor (v 2 3 4 5 6) (.+ (s 1) (v 1 2 3 4 5))))
(print (equal-tensor (v 5 4 3 2 1 0) (.- (v 10 9 8 7 6 5) (s 5))))

(setq pi (s 3.14159))
(setq raio (s 5))
(setq p (.* (s 2) (.* pi raio)))
(setq p (v 20 30 40 50))

(print (equal-tensor (s 100) (.+ (s 1) (s 99))))

(print (equal-tensor (s 0) (.> (s 2) (s 3))))
(print (equal-tensor (s 1) (.< (s 3) (s 5))))

(setq x (v 1 2 3 4 5 6 7))
(print (equal-tensor (v 0 0 0 1 1 1 1) (.> x (s 3))))
(print (equal-tensor (v 1 1 1 1 0 0 0) (.> (s 5) x)))


;;;;;;;;;;;;;;;;;;;;

(print (equal-tensor (s 10) (funcall (fold #'.+) (v 1 2 3 4))))
(print (equal-tensor (s 10) (.+ (s 1) (.+ (s 2) (.+ (s 3) (s 4))))))

(print (equal-tensor (s 24) (funcall (fold #'.*) (v 1 2 3 4))))
(print (equal-tensor (s 24) (.* (s 1) (.* (s 2) (.* (s 3) (s 4))))))

(print (equal-tensor (v 1 3 6 10) (funcall (scan #'.+) (v 1 2 3 4))))

(print (equal-tensor (s 1) (funcall (fold #'.+) (v 1))))
(print (equal-tensor (s 3) (funcall (fold #'.+) (v 1 2))))
(print (equal-tensor (s 6) (funcall (fold #'.+) (v 1 2 3))))
(print (equal-tensor (s 10) (funcall (fold #'.+) (v 1 2 3 4))))

(print (funcall (fold #'.*) (.< x (s 0))))
(print (funcall (fold #'.+) (.> x (s 5))))
(print (funcall (fold #'.+) (.= x x)))

;;;;;;;;;;;;;;;;;;;;

(print (equal-tensor (v 1 2 3 4 5 6 7) (interval 7)))
(print (reshape (v 2 3) (v 1 2 3 4)))
(print (reshape (v 2 3) (interval 4)))
(print (reshape (v 2 2) (v 1)))
(print (reshape (v 2 2 2) (v 1 2 3 4 5)))

;;;;;;;;;;;;;;;;;;;;

(print (equal-tensor (v 3 4 5 6 7 8 9 10) (drop (s 2) (interval 10))))
(print (equal-tensor (v 1 2 3 4 5 6 7 8) (drop (s -2) (interval 10))))

(print (equal-tensor (reshape (v 1 2) (v 4 1 2)) (drop (s 1) (reshape (v 2 3) (interval 4)))))
(print (equal-tensor (reshape (v 1 2) (v 1 2)) (drop (v 1 1) (reshape (v 2 3) (interval 4)))))
(print (equal-tensor (reshape (v 1 2 2) (v 1 2 3 4)) (reshape (v 2 2 2) (v 1 2 3 4 5))))

;;;;;;;;;;;;;;;;;;;;

(setq m1 (reshape (v 2 2) (v 10 20 30 40)))
(setq m2 (reshape (v 2 3) (v 1 2 3 4 5 6)))

(print (equal-tensor (reshape (v 2 3) (v 90 120 150 190 260 330)) (funcall (inner-product #'.+ #'.*) m1 m2)))
(print (equal-tensor (reshape (v 2 3) (v 264 300 338 1364 1440 1518)) (funcall (inner-product #'.* #'.+) m1 m2)))
(print (equal-tensor (reshape (v 2 3) (v 35 37 39 75 77 79)) (funcall (inner-product #'.+ #'.+) m1 m2)))

;;;;;;;;;;;;;;;;;;;;

(print (equal-tensor (reshape (v 10 10) (v 1 2 3 4 5 6 7 8 9 10 2 4 6 8 10 12 14 16 18 20 3 6 9 12 15 18 21 24 27 30 4 8 12 16 20 24 28 32 36 40 5 10 15 20 25 30 35 40 45 50 6 12 18 24 30 36 42 48 54 60 7 14 21 28 35 42 49 56 63 70 8 16 24 32 40 48 56 64 72 80 9 18 27 36 45 54 63 72 81 90 10 20 30 40 50 60 70 80 90 100))
(funcall (outer-product #'.*) (interval 10) (interval 10))))

;;;;;;;;;;;;;;;;;;;


(print (equal-tensor (v -24 -6 -2 -1 -1) (.- (.! (v 4 3 2 1 0)))))
(print (equal-tensor (v 3) (shape (v 1 2 3))))
(print (equal-tensor (v 2 3) (shape (reshape (v 2 3) (v 1 2 3 4 5 6)))))
(print (equal-tensor (v 2) (shape (shape (reshape (v 2 3) (v 1 2 3 4 5 6))))))
(print (equal-tensor (v 1 2 3 4 5 6) (interval 6)))

(print (equal-tensor (reshape (v 3 3) (v 1 2 3 4 5 6 1 2 3)) (reshape (v 3 3) (interval 6))))

(print (equal-tensor (v 5 7 9) (.+ (v 1 2 3) (v 4 5 6))))
(print (equal-tensor (v 5 6 7) (.+ (s 1) (v 4 5 6))))
(print (equal-tensor (v 1 1 0 1 1 0 0) (.> (s 5) (v 3 2 6 1 4 7 5))))
(print (equal-tensor (v 0 1 0) (.< (v 3 1 2) (v 2 3 1))))
(print (equal-tensor (v 0 0 0 1 0 0 0 0 0) (let ((v (v 1 2 3 4 5 6 7 8 9)))
                                                (.and (.< (s 3) v) (.< v (s 5))))))
(print (equal-tensor (reshape (v 2 2 2) (v 1 2 3 1 2 3 1 2)) (reshape (v 2 2 2) (v 1 2 3))))
(print (equal-tensor (v 1 2 3 4 5) (catenate (v 1 2) (v 3 4 5))))
(print (equal-tensor (v 3 4 5 6 7 8 9 10) (drop (s 2) (interval 10))))
(print (equal-tensor (v 1 2 3 4 5 6 7 8) (drop (s -2) (interval 10))))
(print (equal-tensor (reshape (v 2 2) (v 5 6 8 9)) (drop (v 1 1) (reshape (v 3 3) (interval 9)))))
(print (equal-tensor (reshape (v 3 3) (v 1 1 0 0 1 1 0 0 1)) (member? (reshape (v 3 3) (interval 4)) (v 1 2))))
(print (equal-tensor (v 6 7 5 4) (let ((v (v 1 6 2 7 3 0 5 4)))
                                    (select (.> v (s 3)) v))))
(print (equal-tensor (reshape (v 2 2) (v 1 3 4 6)) (select (v 1 0 1) (reshape (v 2 3) (interval 6)))))


;;;;;;;;;;;;;;;;;;;;


(print (equal-tensor (s 10) (funcall (fold #'.+) (v 1 2 3 4))))
(print (equal-tensor (s 24) (funcall (fold #'.*) (v 1 2 3 4))))
(print (equal-tensor (v 1 3 6 10) (funcall (scan #'.+) (v 1 2 3 4))))
(print (equal-tensor (v 1 2 6 24) (funcall (scan #'.*) (v 1 2 3 4))))
(print (equal-tensor (reshape (v 4 4) (v 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)) (funcall (outer-product #'.=) (interval 4) (interval 4))))
(print (equal-tensor (reshape (v 2 3 4) (v 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0))  (funcall (outer-product #'.=) (v 4 7) (reshape (v 3 4) (interval 12)))))
(print (equal-tensor (reshape (v 2 3) (v 90 120 150 190 260 330)) (funcall (inner-product #'.+ #'.*) (reshape (v 2 2) (v 10 20 30 40)) (reshape (v 2 3) (v 1 2 3 4 5 6)))))
(print (equal-tensor (reshape (v 2 3) (v 264 300 338 1364 1440 1518)) (funcall (inner-product #'.* #'.+) (reshape (v 2 2) (v 10 20 30 40)) (reshape (v 2 3) (v 1 2 3 4 5 6)))))

;;;;;;;;;;;;;;;;;;;;;

(print (equal-tensor (s 18) (tally (reshape (v 3 3 2) (interval 5)))))

(print (equal-tensor (s 24) (tally (reshape (v 1 2 3 4) (interval 5)))))

(print (equal-tensor (s 3) (rank (reshape (v 4 5 2) (interval 5)))))

(print (equal-tensor (s 2) (rank (reshape (v 4 5) (interval 5)))))

(print (equal-tensor (v 7 8 6 5) (within (v 2 7 3 1 9 8 4 6 5) (s 5) (s 8))))

(print (equal-tensor (v 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4) (ravel (reshape (v 2 3 4) (interval 10)))))

(print (equal-tensor (v 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) (primes (s 50))))


