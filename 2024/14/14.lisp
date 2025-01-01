(require 'cl-ppcre)
(require 'uiop)
(require 'asdf)
;;(asdf:load-asd (merge-pathnames "zpng.asd" (uiop:getcwd)))
;;(asdf:load-system "zpng")
(push (merge-pathnames "zpng/" (uiop:getcwd)) asdf:*central-registry*)
(require 'zpng)

;;(require 'quicklisp)
;;(pushnew (sb-posix:getcwd) quicklisp:*local-project-directories* :test #'equalp)
;;(ql:quickload "zpng")
;;(push  (sb-posix:getcwd) asdf:*central-registry*)
;;(require 'zpng)

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

(defun inside-board (pos b) (inside-board-dim pos (board-dimensions b)))

(defun char-at (pos b)
  (when (inside-board pos b)
    (aref b (imagpart pos) (realpart pos))))

(defun for-each-pos-on-board (f b)
  (loop for y from 0 to (1- (board-height b)) do
    (loop for x from 0 to (1- (board-width b)) do
      (funcall f (complex x y) (char-at (complex x y) b)))))

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

(defun complex< (a b)
  (or (< (imagpart a) (imagpart b))
      (and (= (imagpart a) (imagpart b))
           (< (realpart a) (realpart b)))))

(defun print-robots (positions)
  (let ((b (make-empty-board (+ #C(1 1) (complex-max positions)))))
    (mapc (lambda (pos) (set-char-at #\* pos b)) positions)
    (print-board b)))

(defun has-horizontal-line (positions)
  (let* ((spos (sort (copy-list positions) #'complex<))
         (line-length 0)
         (has-line nil))
    (mapc (lambda (a b)
            (if (= (- b a) 1)
                (when (> (incf line-length) 10) (setf has-line t))
                (setf line-length 0)))
          spos (cdr spos))
    has-line))

(defun predict-robot-positions (num-steps dim file &key print-func)
  (flet ((move-robot (pos v) (complex-mod (+ pos v) dim)))
    (let* ((robots (read-pos-and-velocity-file file))
           (positions (mapcar #'car robots))
           (velocities (mapcar #'cdr robots)))
      (dotimes (i num-steps)
        (when (has-horizontal-line (copy-list positions)) (format t "Step ~a~%" i))
        (when print-func (funcall print-func i positions))
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

(predict-robot-positions 40000 #C(101 103) "input.txt")


(import 'zpng:pixel-streamed-png)
(import 'zpng:start-png)
(import 'zpng:finish-png)
(import 'zpng:write-pixel)
(import 'zpng:data-array)
(import 'zpng:write-png)

(defun draw-grayscale-png (file dim func)
  (let* ((png (make-instance 'zpng:png
                             :color-type :grayscale
                             :width (realpart dim)
                             :height (imagpart dim))))
    (funcall func (data-array png))
    (write-png png file))
  t)


(defun draw-robot-positions (png-file max-png-dim start-steps input-dim input-file)
  (let* ((period (+ input-dim #C(1 1)))
         (n (complex (floor (1- (realpart max-png-dim)) (realpart period))
                     (floor (1- (imagpart max-png-dim)) (imagpart period))))
         (png-dim (complex (1- (* (realpart n) (realpart period)))
                           (1- (* (imagpart n) (imagpart period)))))
         (num-steps (+ start-steps (* (realpart n) (imagpart n)))))
    (print period)
    (flet ((draw-collage (image)
             (labels ((draw-positions (i positions)
                        (let ((ni (- i start-steps)))
                          ;;(print ni)
                          (when (>= ni 0)
                            (multiple-value-bind (py px) (floor ni (realpart n))
                              ;;(format t "~a,~a~%" px py)
                              (let ((offs (complex (* px (realpart period))
                                                   (* py (imagpart period))))
                                    (a 0);;(if (oddp (+ ni py)) 0 255))
                                    (b 255));;(if (oddp (+ ni py)) 0 255)))
                                ;;(print (list offs input-dim))
                                (loop for dy from 0 to (1- (imagpart input-dim)) do
                                  (loop for dx from 0 to (1- (realpart input-dim)) do
                                    (let ((x (+ dx (realpart offs)))
                                          (y (+ dy (imagpart offs))))
                                      (setf (aref image y x 0) a))))
                                (mapc (lambda (pos)
                                        (let ((p (+ pos offs)))
                                          (setf (aref image (imagpart p) (realpart p) 0) b)))
                                      positions)))))))
               (predict-robot-positions num-steps input-dim input-file :print-func #'draw-positions))))
      (draw-grayscale-png png-file png-dim #'draw-collage))))

;;(draw-robot-positions "hej.png" #C(60 30) 100 #C(11 7) "test.txt")
;;(dotimes (n 100)
;;  (draw-robot-positions (format nil "pos-~a.png" n) #C(1900 1000) (* n 180) #C(101 103) "input.txt"))
