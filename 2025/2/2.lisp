(load "../aoc")
(require 'cl-ppcre)
(require 'uiop)

(defun read-range-file (file)
  (mapcar (lambda (line)
            (cl-ppcre:register-groups-bind
                ((#'parse-integer a) (#'parse-integer b))
                ("(.+)-(.+)" line)
              (cons a b)))
          (uiop:read-file-lines file)))

(defun equal-halves-p (id)
  (let ((id-str (format nil "~a" id)))
    (multiple-value-bind
          (half-len odd) (floor (length id-str) 2)
      (and (= odd 0)
           (string= (subseq id-str 0 half-len)
                    (subseq id-str half-len))))))

(defun expand-range (range)
  (loop for i from (car range) to (cdr range) collect i))

(defun sum-of-ids (pred file)
  (reduce #'+
          (remove-if-not pred
                         (mapcan #'expand-range
                                 (read-range-file file)))))

(defun parts-equal (str num-parts)
  (multiple-value-bind
        (seq-len rem) (floor (length str) num-parts)
    (and (= rem 0)
         (let ((sub0 (subseq str 0 seq-len)))
           (every (lambda (i)
                    (let ((si (* i seq-len)))
                      (string= sub0 (subseq str si (+ si seq-len)))))
                  (loop for i from 1 to (1- num-parts) collect i))))))

(defun only-repetition-p (id)
  (let ((id-str (format nil "~a" id)))
    (labels ((f (num-parts)
               (and (> num-parts 1)
                    (or (parts-equal id-str num-parts)
                        (f (1- num-parts))))))
      (f (length id-str)))))

(aoc (lambda (file) (sum-of-ids #'equal-halves-p file)) 1227775554
     (lambda (file) (sum-of-ids #'only-repetition-p file)) 4174379265)
