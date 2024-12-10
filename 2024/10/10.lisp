(require 'uiop)

(defun prev-char (c)
  (code-char (1- (char-code c))))

(defun sum-chars (list-of-char-digits)
  (let ((z (char-int #\0)))
    (code-char (+ z (reduce #'+ (mapcar (lambda (c) (- (char-int c) z)) list-of-char-digits))))))

(assert (eq #\9 (sum-chars (list #\2 #\3 #\4))))

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

(defun find-char-eq (c b)
  (find-char (lambda (x) (eq x c)) b))

(defun find-char-not-eq (c b)
  (find-char (lambda (x) (not (eq x c))) b))

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (realpart pos)) (terpri))
           (princ (if (eq c #\.) #\· c))))
    (for-each-pos-on-board #'print-char b)))

(defun make-empty-board (dim)
  (make-array (list (imagpart dim) (realpart dim)) :element-type 'character
                                                   :initial-element #\Space))

(defun make-empty-matrix (dim)
  (make-array (list (imagpart dim) (realpart dim)) :initial-element nil))

(defun neighbor-positions (center-pos dim)
  (remove-if-not (lambda (pos) (inside-board-dim pos dim))
                 (mapcar (lambda (d) (+ center-pos d))
                         '(#C(0 1) #C(0 -1) #C(1 0) #C(-1 0)))))

(defun non-empty-neighbors (pos b)
  (flet ((is-empty (pos) (null (char-at pos b))))
    (remove-if #'is-empty (neighbor-positions pos (board-dimensions b)))))

(defun foo (c prev-score-map b)
  (let* ((score-map (make-empty-matrix (board-dimensions b)))
         (score-pos (find-char-eq c b)))
    (mapc (lambda (pos)
            (let ((neighbors (non-empty-neighbors pos prev-score-map)))
              (when neighbors
                (let ((origins (copy-list (mapcar (lambda (pos) (char-at pos prev-score-map)) neighbors))))
                  ;;(let ((flat-origins (mapcan #'identity origins)))
                  (let ((flat-origins (apply #'append origins)))
                    (set-char-at
                     (remove-duplicates
                      flat-origins
                      )
                     pos score-map))
                  ))))
          score-pos)
    score-map))

(defun sum-of-trailhead-scores (file)
  (let* ((b (read-character-matrix-file file))
         (score-map (make-empty-matrix (board-dimensions b)))
         (score-pos (find-char-eq #\9 b)))
    (mapc (lambda (pos) (set-char-at (list pos) pos score-map)) score-pos)
    (mapc (lambda (c)
            (setf score-map (foo c score-map b))
            )
          (list #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))
    (reduce #'+ (mapcar (lambda (pos) (length (char-at pos score-map))) (find-char #'identity score-map)))))

(assert (= (sum-of-trailhead-scores "test.txt") 36))
(print (sum-of-trailhead-scores "input.txt"))
