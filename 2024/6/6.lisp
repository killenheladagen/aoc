(require 'uiop)

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

(defun char-at (pos b)
  (when (inside-board pos b)
    (aref b (cdr pos) (car pos))))

(defun set-char-at (c pos b)
  (setf (aref b (cdr pos) (car pos)) c))

(defun for-each-pos-on-board (f b)
  (loop for y from 0 to (1- (board-height b)) do
    (loop for x from 0 to (1- (board-width b)) do
      (funcall f (cons x y) (char-at (cons x y) b)))))

(defun find-char (pred b)
  (let* ((result)
         (store-if-match (lambda (pos c) (when (funcall pred c) (push pos result)))))
    (for-each-pos-on-board store-if-match b)
    result))

(defun print-board (b)
  (flet ((print-char (pos c)
           (when (zerop (car pos)) (terpri))
           (princ (if (eq c #\.) #\· c))))
    (for-each-pos-on-board #'print-char b)))

(defparameter *trace-bit-weights* ".╾╽┓╼━┏┳╿┛┃┫┗┻┣╋")
(defparameter *dir-weights*       "_<v_>___^")

(defun obstacle-p (c) (find c "#O"))
(defun visited-p (c) (not (find c ".#O")))

(defun trace-from-dir-char (c)
  (ecase c
    (#\^ #\╽)
    (#\> #\╾)
    (#\v #\╿)
    (#\< #\╼)))

(defun add-step (dir pos)
  (ecase dir
    (#\^ (cons (car pos) (1- (cdr pos))))
    (#\> (cons (1+ (car pos)) (cdr pos)))
    (#\v (cons (car pos) (1+ (cdr pos))))
    (#\< (cons (1- (car pos)) (cdr pos)))))

(defun turn-right (dir)
  (let ((cw "^>v<^"))
    (char cw (1+ (position dir cw)))))

(defun opposite-dir (dir)
  (turn-right (turn-right dir)))

(defun mark-trace (dir pos b)
  "Return T if a loop was detected."
  (let ((old-bits (position (char-at pos b) *trace-bit-weights*)))
    (when old-bits
      (let ((new-bits (logior old-bits (position dir *dir-weights*))))
        (set-char-at (char *trace-bit-weights* new-bits) pos b)
        (eq old-bits new-bits)))))

(defun walk-until-obstacle (pos dir b)
  "Return T if a loop was detected."
  (when (inside-board pos b)
    (let* ((pos-ahead (add-step dir pos))
           (char-ahead (char-at pos-ahead b)))
      (if (obstacle-p char-ahead)
          pos
          (or (mark-trace dir pos b) ;; Stop if loop was detected
              (when (inside-board pos-ahead b)
                ;;(mark-trace (opposite-dir dir) pos-ahead b) ;; Would mess up loop detection
                (walk-until-obstacle pos-ahead dir b)))))))

(defun walk-until-off-board (pos dir b)
  "Return T if a loop was detected."
  (or (eq pos t) ;; Stop if loop was detected
      (when (setf pos (walk-until-obstacle pos dir b))
        (setf dir (turn-right dir))
        (walk-until-off-board pos dir b))))

(defun find-start-and-walk-board (b)
  "Return T if a loop was detected."
  (let* ((dir #\^)
         (pos (car (find-char (lambda (c) (eq c dir)) b))))
    (set-char-at #\. pos b)
    (let ((has-loop (walk-until-off-board pos dir b)))
      (when (and has-loop (< (board-width b) 10)) (print-board b))
      has-loop)))

(defun distinct-visits (f)
  (let ((b (read-character-matrix-file f)))
    (find-start-and-walk-board b)
    (find-char #'visited-p b)))

(defun count-distinct-visits (f)
  (length (distinct-visits f)))

(assert (= (count-distinct-visits "test.txt") 41))
(print (count-distinct-visits "input.txt"))

(defun obstruct-board (pos f)
  (let ((b (read-character-matrix-file f)))
    (when (eq (char-at pos b) #\.) (set-char-at #\O pos b))
    b))

(defun obstructions-that-loop (f)
  (count-if #'identity
            (mapcar (lambda (obst-pos)
                      (find-start-and-walk-board (obstruct-board obst-pos f)))
                    (distinct-visits f))))

(assert (= (obstructions-that-loop "test.txt") 6))
(print (obstructions-that-loop "input.txt"))
