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

;;(defun count-unique-elements-in-ranges (file)
;;  (let ((ranges (read-ingredients-file file)))

(aoc (lambda (file) (count-fresh-ingredients file)) 3)
;;(lambda (file) (count-unique-elements-in-ranges file)) 14)
