(require 'uiop)

(defparameter *wide-boxes* nil)

(defun widen-row (row)
  (mapcan (lambda (c)
            (case c
              (#\@ (list #\@ #\.))
              (#\O (list #\[ #\]))
              (otherwise (list c c))))
          row))

(defun read-character-matrix-file (f)
  (let* ((rows (mapcar (lambda (string)
                         (let ((row (coerce string 'list)))
                           (if *wide-boxes* (widen-row row) row)))
                       (uiop:read-file-lines f)))
         (num-cols (length (car rows))))
    (make-array (list (length rows) num-cols) :element-type 'character
                                              :initial-contents rows)))

(defun read-step-file (f)
  (mapcan (lambda (x) (coerce x 'list)) (uiop:read-file-lines f)))

(defun char-dir (c)
  (ecase c
    (#\^ #C(0 -1))
    (#\> 1)
    (#\v #C(0 1))
    (#\< -1)))

(defun read-files (base)
  (values (read-character-matrix-file (format nil "~a.txt" base))
          (mapcar #'char-dir (read-step-file (format nil "~a-v.txt" base)))))

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

(defun set-char-at (c pos b)
  (setf (aref b (imagpart pos) (realpart pos)) c))

(defun for-each-pos-on-board (f b)
  (loop for y from 0 to (1- (board-height b)) do
    (loop for x from 0 to (1- (board-width b)) do
      (funcall f (complex x y) (char-at (complex x y) b)))))

(defun find-char (pred b)
  (let* ((result)
         (store-if-match (lambda (pos c) (when (funcall pred c) (push pos result)))))
    (for-each-pos-on-board store-if-match b)
    result))

(defun find-char-eq (c b)
  (find-char (lambda (x) (eq x c)) b))

(defun find-char-not-eq (c b)
  (find-char (lambda (x) (not (eq x c))) b))

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (realpart pos)) (terpri))
           (princ (if (eq c #\.) #\· c))))
    (for-each-pos-on-board #'print-char b)))

(defun move-char-at (pos step empty-char b)
  (set-char-at (char-at pos b) (+ pos step) b)
  (set-char-at empty-char pos b))

(defun object-extend (c dir)
  (let ((vertical (zerop (realpart dir))))
    (case c
      (#\O '(0))
      (#\[ (if vertical '(0 1) '(0)))
      (#\] (if vertical '(0 -1) '(0)))
      (otherwise nil))))

(defun object-positions (c pos dir)
  (mapcar (lambda (d) (+ pos d)) (object-extend c dir)))

(defun can-move-and-push (pos-list dir b)
  (every (lambda (pos)
           (let* ((pos-ahead (+ pos dir))
                  (c-ahead (char-at pos-ahead b)))
             (unless (eq c-ahead #\#) ;; Wall ahead
               (can-move-and-push
                (object-positions c-ahead pos-ahead dir)
                dir b))))
         pos-list))

(defun move-and-push (pos-list dir b)
  (mapc (lambda (pos)
          (let* ((pos-ahead (+ pos dir))
                 (c-ahead (char-at pos-ahead b)))
            (assert (not (eq c-ahead #\#))) ;; Wall ahead
            (move-and-push
             (object-positions c-ahead pos-ahead dir)
             dir b)
            (move-char-at pos dir #\Space b)))
        pos-list))

(defun move-robot (pos dir b)
  (if (can-move-and-push (list pos) dir b)
      (progn (move-and-push (list pos) dir b)
             dir)
      0))

(defun predict-robot-moves (file)
  (multiple-value-bind (b steps) (read-files file)
    (let ((pos (car (find-char-eq #\@ b))))
      ;;(print-board b)
      (mapc (lambda (dir)
              (incf pos (move-robot pos dir b))
              ;;(format t "~%~a" dir)
              ;;(print-board b)
              )
            steps))
    b))

(defun gps-coord (pos)
  (+ (* 100 (imagpart pos)) (realpart pos)))

(defun sum-of-gps (file)
  (let ((c (if *wide-boxes* #\[ #\O)))
    (reduce #'+ (mapcar #'gps-coord (find-char-eq c (predict-robot-moves file))))))

(assert (= (sum-of-gps "test-small") 2028))
(assert (= (sum-of-gps "test") 10092))
(print (sum-of-gps "input"))

(setf *wide-boxes* t)
(sum-of-gps "test-small-wide")
(assert (= (sum-of-gps "test") 9021))
(print (sum-of-gps "input"))
