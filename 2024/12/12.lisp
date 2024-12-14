(defun read-character-matrix-file (f)
  (let* ((rows (mapcar (lambda (string) (coerce string 'list))
                       (uiop:read-file-lines f)))
         (num-cols (length (car rows))))
    (make-array (list (length rows) num-cols) :element-type 'character
                                              :initial-contents rows)))

(defun board-width (b) (array-dimension b 1))
(defun board-height (b) (array-dimension b 0))
(defun board-dimensions (b) (complex (board-width b) (board-height b)))

(defun inside-board-dim (pos dim)
  (and (>= (realpart pos) 0) (< (realpart pos) (realpart dim))
       (>= (imagpart pos) 0) (< (imagpart pos) (imagpart dim))))

(defun for-each-pos-on-board (f b)
  (loop for y from 0 to (1- (board-height b)) do
    (loop for x from 0 to (1- (board-width b)) do
      (funcall f (complex x y) (char-at (complex x y) b)))))

(defun inside-board (pos b) (inside-board-dim pos (board-dimensions b)))

(defun char-at (pos b)
  (when (inside-board pos b)
    (aref b (imagpart pos) (realpart pos))))

(defun set-char-at (c pos b)
  (setf (aref b (imagpart pos) (realpart pos)) c))

(defparameter *cardinal-dirs* '(#C(0 -1) #C(1 0) #C(0 1) #C(-1 0)))

(defun collect-and-clear-neighbors (c pos b)
  (when (and (inside-board pos b) (eq c (char-at pos b)))
    (set-char-at #\. pos b)
    (cons pos (mapcan (lambda (dir)
                        (collect-and-clear-neighbors c (+ pos dir) b))
                      *cardinal-dirs*))))

(defun build-region-list (b)
  (let ((regions))
    (flet ((f (pos c)
             (unless (eq c #\.)
               (push (list c (collect-and-clear-neighbors c pos b)) regions))))
      (for-each-pos-on-board #'f b))
    regions))

(defun neighbors-in-list (pos list)
  (mapcan (lambda (dir)
            (let ((n (+ pos dir)))
              (when (find n list) (list n))))
          *cardinal-dirs*))

(defun perimeter-length (reg)
  (reduce #'+ (mapcar (lambda (pos)
                        (- 4 (length (neighbors-in-list pos reg))))
                      reg)))

(defun modern-region-price (reg)
  (* (length (cadr reg)) (perimeter-length (cadr reg))))

(defun total-price (price-func file)
  (let* ((b (read-character-matrix-file file))
         (regions (build-region-list b)))
    (reduce #'+ (mapcar price-func regions))))

(assert (= (total-price #'modern-region-price "test-small.txt") 140))
(assert (= (total-price #'modern-region-price "test-holes.txt") 772))
(assert (= (total-price #'modern-region-price "test.txt") 1930))
(print (total-price #'modern-region-price "input.txt"))

(defun max-complex (list)
  (complex (reduce #'max (mapcar #'realpart list))
           (reduce #'max (mapcar #'imagpart list))))

(defun make-empty-board (dim)
  (make-array (list (imagpart dim) (realpart dim)) :element-type 'character
                                                   :initial-element #\.))

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (realpart pos)) (terpri))
           (princ (if (eq c #\.) #\· c))))
    (for-each-pos-on-board #'print-char b)))

(defun turn-right (dir)
  (nth (mod (1+ (position dir *cardinal-dirs*)) 4) *cardinal-dirs*))

(defun pos-ahead (pos dir)
  (+ pos dir))

(defun char-ahead (pos dir b)
  (char-at (pos-ahead pos dir) b))

(defun empty-ahead (pos dir b)
  (eq (char-ahead pos dir b) #\.))

(defun right-side-pos (pos dir)
  (+ pos (turn-right dir)))

(defun char-to-the-right (pos dir b)
  (char-at (right-side-pos pos dir) b))

(defun wall-to-the-right (pos dir b)
  (not (eq (char-to-the-right pos dir b) #\.)))

(defun find-start-outside-reg (b)
  (labels ((f (pos)
             (if (inside-board pos b)
                 (if (eq (char-at pos b) #\*) (1- pos) (f (1+ pos)))
                 (f (complex 0 (1+ (imagpart pos)))))))
    (f 0)))

(defun follow-perimeter (pos dir b)
  (set-char-at #\# pos)
  (
   (let ((pos )
         (dir #C(0 -1)))
     (format t "start at ~a dir ~a~%" pos dir)


     0))

  (defun number-of-sides (reg)
    (let* ((b (make-empty-board (+ (max-complex reg) #C(3 3)))))
      (mapcar (lambda (pos) (set-char-at #\* (+ pos #C(1 1)) b)) reg)
      (print-board b)
      (follow-perimeter (find-start-outside-reg b) #C(0 -1) b)))

  (defun bulk-region-price (reg)
    (format t "bulk-price-region ~a~%" reg)
    (* (length (cadr reg)) (number-of-sides (cadr reg))))

  (assert (= (total-price #'bulk-region-price "test-small.txt") 80))
  (assert (= (total-price #'bulk-region-price "test-holes.txt") 436))
  (assert (= (total-price #'bulk-region-price "test-e-shape.txt") 236))
  (assert (= (total-price #'bulk-region-price "test-ab.txt") 368))
  (assert (= (total-price #'bulk-region-price "test.txt") 1206))
  (print (total-price #'bulk-region-price "input.txt"))
