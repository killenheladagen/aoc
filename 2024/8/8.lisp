(require 'uiop)

(defun map-pairs-using (mapf f list)
  (mapcon (lambda (sublist)
            (funcall mapf (lambda (b)
                            (funcall f (car sublist) b))
                     (cdr sublist)))
          list))

(defun map-pairs-con (f list)
  (map-pairs-using #'mapcan f list))

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

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (realpart pos)) (terpri))
           (princ (if (eq c #\.) #\· c))))
    (for-each-pos-on-board #'print-char b)))

(defun antenna-table (b)
  (let ((antennas (make-hash-table)))
    (flet ((store-if-antenna (pos c)
             (unless (eq c #\.)
               (push pos (gethash c antennas)))))
      (for-each-pos-on-board #'store-if-antenna b)
      antennas)))

(defun resonance-positions (a b dim)
  (let* ((d (- b a)))
    (remove-if-not (lambda (pos) (inside-board-dim pos dim)) (list (- a d) (+ b d)))))

(defun antinode-map (antinode-func file)
  (let* ((b (read-character-matrix-file file))
         (dim (board-dimensions b))
         (antennas (antenna-table b))
         (draw-dash (lambda (pos) (when (inside-board pos b) (set-char-at #\# pos b)))))
    (loop for v being the hash-value of antennas do
      (mapc draw-dash (map-pairs-con (lambda (a b) (funcall antinode-func a b dim)) v)))
    b))

(defun antinode-positions (antinode-func file)
  (find-char (lambda (c) (eq c #\#)) (antinode-map antinode-func file)))

(defun count-unique-antinodes (antinode-func file)
  (length (antinode-positions antinode-func file)))

(assert (= (count-unique-antinodes #'resonance-positions "test.txt") 14))
(print (count-unique-antinodes #'resonance-positions "input.txt"))
