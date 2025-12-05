(load "../aoc")
(require 'cl-ppcre)
(require 'uiop)

(defun read-ingredients-file (file)
  (let ((ranges)
        (ids)
        (empty-line-found))
    (mapc (lambda (line)
            (cond
              ((= (length line) 0) (setf empty-line-found t))
              (empty-line-found (push (parse-integer line) ids))
              (t (push (cl-ppcre:register-groups-bind
                           ((#'parse-integer a) (#'parse-integer b))
                           ("(.+)-(.+)" line)
                         (cons a b))
                       ranges))))
          (uiop:read-file-lines file))
    (values ranges ids)))

(defun in-range (elem range)
  (<= (car range) elem (cdr range)))

(defun count-fresh-ingredients (file)
  (multiple-value-bind (ranges ids) (read-ingredients-file file)
    (flet ((fresh-p (id)
             (remove-if-not (lambda (x) (in-range id x)) ranges)))
      (length (remove-if-not #'fresh-p ids)))))

(defun range-size (x)
  (1+ (- (cdr x) (car x))))

(defun range-eq (a b)
  (and (= (car a) (car b))
       (= (cdr a) (cdr b))))

(defun range-combine (a b)
  (when (or (in-range (car a) b)
            (in-range (cdr a) b)
            (in-range (car b) a)
            (in-range (cdr b) a))
    (cons (min (car a) (car b))
          (max (cdr a) (cdr b)))))

(defun merge-overlapping-ranges (ranges &optional disjunct-ranges)
  (if ranges
      (let* ((first-range (car ranges))
             (new-ranges (remove-if #'null
                                    (mapcar
                                     (lambda (x)
                                       (let ((new-first (range-combine first-range x)))
                                         (if new-first
                                             (progn (setf first-range new-first)
                                                    nil)
                                             x)))
                                     (cdr ranges)))))
        (if (range-eq first-range (car ranges))
            (merge-overlapping-ranges new-ranges (cons first-range disjunct-ranges))
            (merge-overlapping-ranges (cons first-range new-ranges) disjunct-ranges)))
      disjunct-ranges))

(defun count-unique-elements-in-ranges (file)
  (let ((ranges (read-ingredients-file file)))
    (reduce #'+ (mapcar #'range-size (merge-overlapping-ranges ranges)))))

(aoc (lambda (file) (count-fresh-ingredients file)) 3
     (lambda (file) (count-unique-elements-in-ranges file)) 14)
