(load "board")

(defun taxicab-offsets (min max)
  (let ((r (loop for i from 0 to max collect i)))
    (remove-duplicates
     (mapcan (lambda (c)
               (list (complex (- (realpart c)) (imagpart c))
                     (complex (realpart c) (- (imagpart c))) c (- c)))
             (remove-if-not (lambda (c) (<= min (+ (realpart c) (imagpart c)) max))
                            (mapcan (lambda (x) (mapcar (lambda (y) (complex x y)) r)) r))))))

(defun numeric-neighbors (center-pos offsets b)
  (remove-if-not #'numberp (mapcar (lambda (pos) (at pos b))
                                   (add-offset-inside-dim center-pos (board-dimensions b) offsets))))

(defun num-cheats-to (pos cheat-distance cheat-offsets b)
  (remove-if-not #'plusp
                 (let ((to-time (at pos b)))
                   (mapcar (lambda (from-time)
                             (- to-time from-time cheat-distance))
                           (numeric-neighbors pos cheat-offsets b)))))

(defun follow-path-and-record-cheats (pos end time cheat-distance cheat-offsets b)
  (set-at time pos b)
  (append (num-cheats-to pos cheat-distance cheat-offsets b)
          (unless (= pos end)
            (let ((next (empty-neighbors pos b)))
              (assert (= (length next) 1))
              (follow-path-and-record-cheats (car next) end (1+ time) cheat-distance cheat-offsets b)))))

(defun number-of-cheats (threshold cheat-distance file)
  (let* ((b (read-board-file file))
         (start (car (find-eq #\S b)))
         (end (car (find-eq #\E b))))
    (length (remove-if (lambda (x) (< x threshold))
                       (follow-path-and-record-cheats start end 0 cheat-distance
                                                      (taxicab-offsets 2 cheat-distance) b)))))

(assert (= (print (number-of-cheats 6 2 "test.txt")) (+ 2 4 2 3 5)))
(assert (= (print (number-of-cheats 100 2 "input.txt")) 1438))

(assert (= (print (number-of-cheats 50 20 "test.txt")) (+ 32 31 29 39 25 23 20 19 12 14 12 22 4 3)))

;;(print (number-of-cheats 100 20 "input.txt"))
