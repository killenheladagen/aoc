(require 'cl-ppcre)
(require 'uiop)

(defun read-csv-and-rows (file)
  (let ((rows (uiop:read-file-lines file)))
    (values (cl-ppcre:split ", " (car rows)) (cddr rows))))

(defun starts-with (whole start)
  (and (>= (length whole) (length start)) (string= (subseq whole 0 (length start)) start)))

(compile 'starts-with)

(defun possible-design (design towels)
  ;;(format t "design=~a towels=~a~%" design (length towels))
  (or (= (length design) 0)
      (remove-if #'null
                 (mapcar (lambda (towel-length)
                           (possible-design (subseq design towel-length) towels))
                         (mapcar #'length (remove-if-not (lambda (x) (starts-with design x)) towels))))))

(compile 'possible-design)

(defun prune-towels (towels)
  (mapcan (lambda (x) (unless (possible-design x (remove x towels :test #'string=)) (list x))) towels))

(defun num-possible-designs (file)
  (multiple-value-bind (towels designs) (read-csv-and-rows file)
    (setf towels (prune-towels towels))
    ;;(format t "~a designs. ~a towels. ~%" (length designs) (length towels))
    (let ((design-results (mapcar (lambda (x) (possible-design x towels)) designs)))
      (length (remove-if #'null design-results)))))

(compile 'num-possible-designs)

(assert (= (num-possible-designs "test.txt") 6))
(print (num-possible-designs "input.txt"))
