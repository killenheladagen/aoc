(require 'cl-ppcre)
(require 'uiop)

(defun read-csv-and-rows (file)
  (let ((rows (uiop:read-file-lines file)))
    (values (cl-ppcre:split ", " (car rows)) (cddr rows))))

(defun starts-with (whole start)
  (and (>= (length whole) (length start)) (string= (subseq whole 0 (length start)) start)))

(defun possible-design (design towels &optional trace)
  ;;(format t "design=~a towels=~a~%" design (length towels))
  (if (= (length design) 0)
      (list (format nil "~{~a~}" (reverse trace)))
      (remove-if #'null
                 (mapcan (lambda (towel)
                           (possible-design (subseq design (length towel)) towels (cons towel trace)))
                         (remove-if-not (lambda (x) (starts-with design x)) towels)))))

(defun all-possible-designs (design towels &optional trace)
  ;;(format t "design=~a towels=~a~%" design (length towels))
  (if (= (length design) 0)
      (list trace)
      (mapcan (lambda (x)
                (when (starts-with design x)
                  (all-possible-designs (subseq design (length x)) towels (cons x trace))))
              towels)))

(defun prune-towels (towels)
  (mapcan (lambda (x) (unless (possible-design x (remove x towels :test #'string=)) (list x))) towels))

(defun num-possible-designs (file)
  (multiple-value-bind (towels designs) (read-csv-and-rows file)
    (let* ((fewer-towels (prune-towels towels))
           (design-results (mapcar (lambda (x) (possible-design x fewer-towels)) designs))
           (fewer-designs (print (mapcan (lambda (x) (remove-duplicates x :test #'string=)) design-results)))
           (unique-design-results (mapcar (lambda (x) (print (all-possible-designs x towels))) fewer-designs)))
      (values (length (remove-if #'null design-results))
              (reduce #'+ (mapcar #'length unique-design-results))))))

(compile 'starts-with)
(compile 'possible-design)
(compile 'all-possible-designs)
(compile 'num-possible-designs)

(multiple-value-bind (n u) (num-possible-designs "test.txt")
  (assert (= n 6))
  (assert (= u 16)))

(multiple-value-bind (n u) (num-possible-designs "input.txt")
  (print n)
  (print u))

;;(print (num-possible-designs "input.txt"))
