(load "board")

(defun cost (path)
  (labels ((f (p dir)
             (if (< (length p) 2)
                 0
                 (+ (if (= (+ (car p) dir) (cadr p)) 1 1001)
                    (f (cdr p) (- (cadr p) (car p)))))))
    (f (reverse path) 1)))

(let ((min-cost))
  (defun paths-to (end cost path b)
    (flet ((d0 (list) (- (cadr list) (car list))))
      (incf cost (if (cdr path)
                     (if (= (d0 path)
                            (if (cddr path)
                                (d0 (cdr path))
                                1)) ;; Start in eastern dir
                         1 1001)
                     0)))
    (when (or (not min-cost) (< cost min-cost))
      (if (= end (car path))
          (progn
            (print path)
            (print (cost path))
            (setf min-cost (min cost (or min-cost cost)))
            (list path))
          (flet ((follow-path (pos)
                   (paths-to end cost (cons pos path) b))
                 (wall-or-visited (pos)
                   (or (eq (at pos b) #\#) (find pos path)))
                 (step-in-dir (dir) (+ (car path) dir)))
            (mapcan #'follow-path
                    (remove-if #'wall-or-visited
                               (mapcar #'step-in-dir *north-east-south-west*)))))))

  (defun lowest-reindeer-score (file)
    (setf min-cost nil)
    (let* ((b (read-board-file file))
           (start (car (find-eq #\S b)))
           (end (car (find-eq #\E b))))
      ;;(reduce #'min (mapcar #'cost (paths-to end (list start) b)))))
      (paths-to end 0 (list start) b))
    min-cost))

(assert (= (lowest-reindeer-score "test.txt") 7036))
(assert (= (lowest-reindeer-score "test2.txt")  11048))
(print (lowest-reindeer-score "input.txt"))
