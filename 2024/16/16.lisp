(load "board")

(defun paths-to (end path b)
  (if (= end (car path))
      (list path)
      (flet ((follow-path (pos)
               (paths-to end (cons pos path) b))
             (wall-or-visited (pos)
               (or (eq (at pos b) #\#) (find pos path)))
             (step-in-dir (dir) (+ (car path) dir)))
        (mapcan #'follow-path
                (remove-if #'wall-or-visited
                           (mapcar #'step-in-dir *north-east-south-west*))))))
(compile 'paths-to)

(defun cost (path)
  (labels ((f (p dir)
             (if (< (length p) 2)
                 0
                 (+ (if (= (+ (car p) dir) (cadr p)) 1 1001)
                    (f (cdr p) (- (cadr p) (car p)))))))
    (f (reverse path) 1)))
(compile 'cost)

(defun lowest-reindeer-score (file)
  (let* ((b (read-board-file file))
         (start (car (find-eq #\S b)))
         (end (car (find-eq #\E b))))
    (reduce #'min (mapcar #'cost (paths-to end (list start) b)))))
(compile 'lowest-reindeer-score)

(assert (= (lowest-reindeer-score "test.txt") 7036))
(assert (= (lowest-reindeer-score "test2.txt")  11048))
(print (lowest-reindeer-score "input.txt"))
