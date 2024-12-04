(defun read-character-matrix-file (f)
  (let* ((rows (mapcar (lambda (string) (coerce string 'list))
                       (uiop:read-file-lines f)))
         (num-cols (length (car rows))))
    (make-array (list (length rows) num-cols) :element-type 'character
                                              :initial-contents rows)))

(defparameter *all-dirs*
  (flet ((cons-zerop (d) (and (zerop (car d)) (zerop (cdr d)))))
    (let ((offs '(-1 0 1)))
      (remove-if #'cons-zerop (mapcan (lambda (dx) (mapcan (lambda (dy) (list (cons dx dy))) offs)) offs)))))

(defun has-word (word board x y d &optional (offs 0))
  (incf x (* offs (car d)))
  (incf y (* offs (cdr d)))
  (setf offs 0)
  (or (zerop (length word))
      (and (>= x 0) (< x (array-dimension board 1))
           (>= y 0) (< y (array-dimension board 0))
           (eq (char word 0) (aref board y x))
           (has-word (subseq word 1) board (+ x (car d)) (+ y (cdr d)) d))))

(defun count-word (word board)
  (let ((count 0))
    (loop for y from 0 to (1- (array-dimension board 0)) do
      (loop for x from 0 to (1- (array-dimension board 1)) do
        (mapc (lambda (d)
                (when (has-word word board x y d)
                  (incf count)))
              *all-dirs*)))
    count))

(defun count-xmas (f)
  (count-word "XMAS" (read-character-matrix-file f)))

(assert (= (count-xmas "test.txt") 18))
(print (count-xmas "input.txt"))

(defun count-cross-word (word board)
  (let* ((offs (- (/ (1- (length word)) 2)))
         (count 0))
    (loop for y from 0 to (1- (array-dimension board 0)) do
      (loop for x from 0 to (1- (array-dimension board 1)) do
        (mapc (lambda (ds)
                (when (notany #'null (mapcar (lambda (d) (has-word word board x y d offs)) ds))
                  (incf count)))
              '(((1 . 1)(1 . -1))((1 . 1)(-1 . 1))
                ((-1 . -1)(1 . -1))((-1 . -1)(-1 . 1))))))
    count))

(defun count-x-mas (f)
  (count-cross-word "MAS" (read-character-matrix-file f)))

(assert (= (count-x-mas "test.txt") 9))
(print (count-x-mas "input.txt"))
