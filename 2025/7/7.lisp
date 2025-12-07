(load "../aoc")
(require 'uiop)

(defun combine-timelines (beams)
  (let ((positions (remove-duplicates (mapcar #'car beams))))
    (mapcar (lambda (pos)
              (flet ((timelines-at-pos (beam)
                       (if (= (car beam) pos) (cdr beam) 0)))
                (cons pos (reduce #'+ (mapcar #'timelines-at-pos beams)))))
            positions)))

(defun count-splitters-used (file)
  (let* ((lines (uiop:read-file-lines file))
         (beams (list (cons (position #\S (car lines) :test #'equal) 1)))
         (num-splits 0))
    (flet ((trace-beams (line)
             (flet ((split-and-count (beam)
                      (if (equal (char line (car beam)) #\^)
                          (progn
                            (incf num-splits)
                            (mapcar (lambda (d) (cons (+ (car beam) d) (cdr beam))) '(1 -1)))
                          (list beam))))
               (setf beams (combine-timelines (mapcan #'split-and-count beams))))))
      (mapc #'trace-beams (cdr lines)))
    (cons num-splits (reduce #'+ (mapcar #'cdr beams)))))

(aoc (lambda (file) (car (count-splitters-used file))) 21
     (lambda (file) (cdr (count-splitters-used file))) 40)
