(require 'cl-ppcre)
(require 'uiop)

(defun read-pos-and-velocity-file (f)
  (mapcar (lambda (row)
            (cl-ppcre:register-groups-bind ((#'parse-integer px py vx vy))
                ("p=(.+),(.+) v=(.+),(.+)" row)
              (cons (complex px py) (complex vx vy))))
          (uiop:read-file-lines f)))

(defun board-width (b) (array-dimension b 1))
(defun board-height (b) (array-dimension b 0))
(defun board-dimensions (b) (complex (board-width b) (board-height b)))

(defun inside-board-dim (pos dim)
  (and (>= (realpart pos) 0) (< (realpart pos) (realpart dim))
       (>= (imagpart pos) 0) (< (imagpart pos) (imagpart dim))))

(defun char-at (pos b)
  (when (inside-board pos b)
    (aref b (imagpart pos) (realpart pos))))

(defun for-each-pos-on-board (f b)
  (loop for y from 0 to (1- (board-height b)) do
    (loop for x from 0 to (1- (board-width b)) do
      (funcall f (complex x y) (char-at (complex x y) b)))))

(defun inside-board (pos b) (inside-board-dim pos (board-dimensions b)))

(defun make-empty-board (dim)
  (make-array (list (imagpart dim) (realpart dim)) :element-type 'character
                                                   :initial-element #\Space))

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (realpart pos)) (terpri))
           (princ (if (eq c #\.) #\· c))))
    (for-each-pos-on-board #'print-char b)))

(defun set-char-at (c pos b)
  (setf (aref b (imagpart pos) (realpart pos)) c))

(defun complex-mod (number divisor)
  (complex (mod (realpart number) (realpart divisor))
           (mod (imagpart number) (imagpart divisor))))

(defun complex-max (numbers)
  (reduce (lambda (a b)
            (complex (max (realpart a) (realpart b))
                     (max (imagpart a) (imagpart b))))
          numbers))

(defun print-robots (positions)
  (let ((b (make-empty-board (+ #C(1 1) (complex-max positions)))))
    (mapc (lambda (pos) (set-char-at #\* pos b)) positions)
    (print-board b)))

(defun predict-robot-positions (num-steps dim file &key print)
  (flet ((move-robot (pos v) (complex-mod (+ pos v) dim)))
    (let* ((robots (read-pos-and-velocity-file file))
           (positions (mapcar #'car robots))
           (velocities (mapcar #'cdr robots)))
      (dotimes (i num-steps)
        (when (and print (>= i 240))
          (format t "~%Number of moves: ~a~%" i)
          (print-robots positions)
          ;;(read-line))
          (sleep 0.5))
        (setf positions (mapcar #'move-robot positions velocities)))
      positions)))

(defun quadrant-limits (dim)
  (let* ((mx (realpart dim))
         (my (imagpart dim))
         (qx (floor mx 2))
         (qy (floor my 2)))
    (list (cons          0          (complex qx qy)) (cons          (1+ qx)          (complex mx qy))
          (cons (complex 0 (1+ qy)) (complex qx my)) (cons (complex (1+ qx) (1+ qy))          dim))))

(defun count-if-inside (area positions)
  (flet ((inside-area (pos)
           (inside-board-dim (- pos (car area)) (- (cdr area) (car area)))))
    (count-if #'inside-area positions)))

(defun quadrant-counts (dim positions)
  (mapcar (lambda (area) (count-if-inside area positions)) (quadrant-limits dim)))

(defun safety-factor (num-steps dim file)
  (reduce #'* (quadrant-counts dim (predict-robot-positions num-steps dim file))))

(assert (eq (safety-factor 100 #C(11 7) "test.txt") 12))
(print (safety-factor 100 #C(101 103) "input.txt"))

(predict-robot-positions 400 #C(101 103) "input.txt" :print t)
