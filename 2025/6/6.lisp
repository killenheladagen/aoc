(load "../aoc")
(require 'cl-ppcre)
(require 'uiop)

(defun array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (aref array i)))

(defun read-problem-file (file)
  (flet ((parse (x)
           (or (parse-integer x :junk-allowed t) (intern x))))
    (let ((problems))
      (mapcar (lambda (line)
                (let* ((raw (remove-if (lambda (x) (string= x "")) (cl-ppcre:split " " line)))
                       (parsed (mapcar #'parse raw)))
                  (unless problems
                    (setf problems (make-array (length parsed) :initial-element nil)))
                  (dotimes (i (length parsed))
                    (push (nth i parsed) (aref problems i)))))
              (uiop:read-file-lines file))
      (array-to-list problems))))

(defun read-file-columns (file)
  (let ((columns))
    (mapc (lambda (line)
            (unless columns
              (setf columns (make-array (length line) :initial-element nil)))
            (dotimes (i (length line))
              (push (char line i) (aref columns i))))
          (uiop:read-file-lines file))
    (mapcar (lambda (x) (format nil "~{~a~}" (reverse x))) (array-to-list columns))))

(defun read-problem-file-col-wise (file)
  (let ((problems)
        (op)
        (p))
    (mapc (lambda (col)
            (let ((num (parse-integer col :junk-allowed t)))
              (if num
                  (let ((last (uiop:last-char col)))
                    (unless (eq last #\Space)
                      (setf op (intern (string last))))
                    (push num p))
                  (progn (push (append (list op) p) problems)
                         (setf p nil)))))
          (append (read-file-columns file) '("")))
    problems))

(defun problem-grand-total (problems)
  (reduce #'+ (mapcar (lambda (p) (apply #'funcall p)) problems)))

(aoc (lambda (file) (problem-grand-total (read-problem-file file))) 4277556
     (lambda (file) (problem-grand-total (read-problem-file-col-wise file))) 3263827)
