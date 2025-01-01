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

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (realpart pos)) (terpri))
           (princ (if (eq c #\.) #\· c))))
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

(defun turn-right (dir)
  (nth (mod (1+ (position dir *north-east-south-west*)) 4) *north-east-south-west*))

(defun turn-left (dir)
  (nth (mod (1- (position dir *north-east-south-west*)) 4) *north-east-south-west*))
