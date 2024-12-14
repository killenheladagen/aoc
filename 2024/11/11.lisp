(require 'uiop)

(defun transform-stone (x)
  (let ((xs (format nil "~a" x)))
    (cond ((zerop x) (list 1))
          ((evenp (length xs))
           (let ((half (/ (length xs) 2)))
             (mapcar #'parse-integer (list (subseq xs 0 half) (subseq xs half)))))
          (t (list (* 2024 x))))))

(let ((cache make-hash-table))
  (defun stone-after (blinks x)
    (let ((cache-ref (gethash x cache)))
      (unless cache-ref
        (setf cache-ref (make-array 76 :initial-element nil))
        (setf (gethash x cache) cache-ref))
      (if (< blinks (length cache-ref))
          (nth blinks cache-ref)
          (progn
            (let ((blink-list
                    (mapcar (lambda (n)
                              (
                               (setf cache-ref blink-list)


                               (defun stones-after (blinks stone-list)
                                 (format t "~a ~a~%" blinks (length stone-list))
                                 (flet ((f (x)
                                          (let ((xs (format nil "~a" x)))
                                            (cond ((zerop x) (list 1))
                                                  ((evenp (length xs))
                                                   (let ((half (/ (length xs) 2)))
                                                     (mapcar #'parse-integer (list (subseq xs 0 half) (subseq xs half)))))
                                                  (t (list (* 2024 x)))))))
                                   (if (zerop blinks)
                                       stone-list
                                       (stones-after (1- blinks) (mapcan #'f stone-list)))))

                               (compile 'stones-after)

                               (assert (equal (stones-after 6 '(125 17))
                                              '(2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2)))
                               (assert (= (length (stones-after 25 '(125 17))) 55312))
                               (print (length (stones-after 25 '(77 515 6779622 6 91370 959685 0 9861))))
                               (print (length (stones-after 75 '(77 515 6779622 6 91370 959685 0 9861))))
