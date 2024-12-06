(defun read-character-matrix-file (f)
  (let* ((rows (mapcar (lambda (string) (coerce string 'list))
                       (uiop:read-file-lines f)))
         (num-cols (length (car rows))))
    (make-array (list (length rows) num-cols) :element-type 'character
                                              :initial-contents rows)))

(defun board-width (b) (array-dimension b 1))
(defun board-height (b) (array-dimension b 0))

(defun inside-board (pos b)
  (and (>= (car pos) 0) (< (car pos) (board-width b))
       (>= (cdr pos) 0) (< (cdr pos) (board-height b))))

(defun for-each-pos-on-board (f b)
  (loop for y from 0 to (1- (board-height b)) do
    (loop for x from 0 to (1- (board-width b)) do
      (funcall f (cons x y) (char-at (cons x y) b)))))

(defun find-char (needle b)
  (let* ((result)
         (store-if-eq (lambda (pos c) (when (eq c needle) (push pos result)))))
    (for-each-pos-on-board store-if-eq b)
    result))

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (car pos)) (terpri))
           (princ c)))
    (for-each-pos-on-board #'print-char b)))

(defun char-at (pos b)
  (when (inside-board pos b)
    (aref b (cdr pos) (car pos))))

(defun set-char-at (c pos b)
  (setf (aref b (cdr pos) (car pos)) c))

(defun turn-char-right (c)
  (ecase c
    (#\^ #\>)
    (#\> #\v)
    (#\v #\<)
    (#\< #\^)))

(defun dir-from-char (c)
  (ecase c
    (#\^ '(0 . -1))
    (#\> '(1 . 0))
    (#\v '(0 . 1))
    (#\< '(-1 . 0))))

(defun add-step (delta pos)
  (cons (+ (car delta) (car pos)) (+ (cdr delta) (cdr pos))))

(defun turn-right (pos b)
  (let ((c (turn-char-right (char-at pos b))))
    (set-char-at c pos b)))

(defun walk-until-char-ahead (stop-char pos b)
  (when (inside-board pos b)
    (let* ((guard-char (char-at pos b))
           (pos-ahead (add-step (dir-from-char guard-char) pos))
           (char-ahead (char-at pos-ahead b)))
      (if (eq char-ahead stop-char)
          pos
          (progn (set-char-at #\X pos b)
                 (when (inside-board pos-ahead b)
                   (set-char-at guard-char pos-ahead b)
                   (walk-until-char-ahead stop-char pos-ahead b)))))))

(defun walk-until-off-board (pos b)
  (when (setf pos (walk-until-char-ahead #\# pos b))
    (turn-right pos b)
    (walk-until-off-board pos b)))

(defun distinct-visits (f)
  (let* ((b (read-character-matrix-file f))
         (start (car (find-char #\^ b))))
    (walk-until-off-board start b)
    (length (find-char #\X b))))

(assert (= (distinct-visits "test.txt") 41))
(print (distinct-visits "input.txt"))
