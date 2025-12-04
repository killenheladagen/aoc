(require 'uiop)

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

(defparameter *8-dirs* '(#C(0 -1) 1 #C(0 1) -1 #C(-1 -1) #C(-1 1) #C(1 -1) #C(1 1)))

(defun remove-if-not-inside-dim (dim pos-list)
  (remove-if-not (lambda (pos) (inside-board-dim pos dim)) pos-list))

(defun add-offset-inside-dim (center-pos dim pos-list)
  (remove-if-not-inside-dim dim (mapcar (lambda (d) (+ center-pos d)) pos-list)))

(defun neighbor-positions (center-pos dim)
  (add-offset-inside-dim center-pos dim *8-dirs*))
