(load "board")

(defun numeric-neighbors-neighbor (center-pos b)
  (let ((nn-pos (neighbors-neighbor-positions center-pos (board-dimensions b))))
    (remove-if-not #'numberp (mapcar (lambda (pos) (at pos b)) nn-pos))))

(defun num-cheats-to (pos b)
  (remove-if-not #'plusp
                 (let ((to-time (at pos b)))
                   (mapcar (lambda (from-time)
                             (- to-time from-time 2))
                           (numeric-neighbors-neighbor pos b)))))

(defun follow-path-and-record-cheats (pos end time b)
  (set-at time pos b)
  (append (num-cheats-to pos b)
          (unless (= pos end)
            (let ((next (empty-neighbors pos b)))
              (assert (= (length next) 1))
              (follow-path-and-record-cheats (car next) end (1+ time) b)))))

(defun number-of-cheats (threshold file)
  (let* ((b (read-board-file file))
         (start (car (find-eq #\S b)))
         (end (car (find-eq #\E b))))
    (length (remove-if (lambda (x) (< x threshold)) (follow-path-and-record-cheats start end 0 b)))))

(assert (= (print (number-of-cheats 6 "test.txt")) (+ 2 4 2 3 5)))
(assert (= (print (number-of-cheats 100 "input.txt")) 1438))
