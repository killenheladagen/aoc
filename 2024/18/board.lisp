(require 'uiop)

(defun make-empty-board (dim &key initial-element)
  (make-array (list (imagpart dim) (realpart dim)) :initial-element initial-element))

(defun read-board-file (f)
  (let* ((rows (mapcar (lambda (string) (coerce string 'list))
                       (uiop:read-file-lines f)))
         (num-cols (length (car rows))))
    (make-array (list (length rows) num-cols) :initial-contents rows)))

(defun board-width (b) (array-dimension b 1))
(defun board-height (b) (array-dimension b 0))
(defun board-dimensions (b) (complex (board-width b) (board-height b)))

(defun inside-board-dim (pos dim)
  (and (>= (realpart pos) 0) (< (realpart pos) (realpart dim))
       (>= (imagpart pos) 0) (< (imagpart pos) (imagpart dim))))

(defun inside-board (pos b) (inside-board-dim pos (board-dimensions b)))

(defun outside-board (pos b) (not (inside-board pos b)))

(defun at (pos b)
  (when (inside-board pos b)
    (aref b (imagpart pos) (realpart pos))))

(defun set-at (c pos b)
  (setf (aref b (imagpart pos) (realpart pos)) c))

(defun for-each-pos-on-board (f b)
  (loop for y from 0 to (1- (board-height b)) do
    (loop for x from 0 to (1- (board-width b)) do
      (let ((pos (complex x y)))
        (funcall f pos (at pos b))))))

(defun print-char-cell (c)
  (format t "~a" (if (eq c #\.) #\· c)))

(defun print-int-cell (c)
  (if c (format t "~3d" c) (format t " ##")))

(defun print-board (b &optional (print-func #'print-char-cell))
  (flet ((print-char (pos c)
           (when (zerop (realpart pos)) (format t "~%"))
           (funcall print-func c)))
    (for-each-pos-on-board #'print-char b)))

(defun find-on-board (pred b)
  (let* ((result)
         (store-if-match (lambda (pos c) (when (funcall pred c) (push pos result)))))
    (for-each-pos-on-board store-if-match b)
    result))

(defun find-eq (c b)
  (find-on-board (lambda (x) (eq x c)) b))

(defun find-not-eq (c b)
  (find-on-board (lambda (x) (not (eq x c))) b))

(defparameter *north-east-south-west* '(#C(0 -1) 1 #C(0 1) -1))

(defun step-char (step)
  (char "^>v<" (position step *north-east-south-west*)))

(defun remove-if-not-inside-dim (dim pos-list)
  (remove-if-not (lambda (pos) (inside-board-dim pos dim)) pos-list))

(defun add-offset-inside-dim (center-pos dim pos-list)
  (remove-if-not-inside-dim dim (mapcar (lambda (d) (+ center-pos d)) pos-list)))

(defun neighbor-positions (center-pos dim)
  (add-offset-inside-dim center-pos dim *north-east-south-west*))

(defun neighbors-neighbor-positions (center-pos dim)
  (add-offset-inside-dim center-pos dim
                         '(#C(0 -2) #C(1 -1) 2 #C(1 1) #C(0 2) #C(-1 1) -2 #C(-1 -1))))

(defun empty-neighbors (pos b)
  (flet ((is-empty (pos) (case (at pos b) ((nil #\. #\E) t))))
    (remove-if-not #'is-empty (neighbor-positions pos (board-dimensions b)))))

(defun turn-right (dir)
  (nth (mod (1+ (position dir *north-east-south-west*)) 4) *north-east-south-west*))

(defun turn-left (dir)
  (nth (mod (1- (position dir *north-east-south-west*)) 4) *north-east-south-west*))
