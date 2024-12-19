
(defun program-output (prg &key (a 0) (b 0) (c 0) (abort-if (lambda (_) (declare (ignore _)) nil)))
  (let ((pc 0)
        (output))
    (flet ((next-inst () (digit-char-p (char prg (* 2 (1- (incf pc))))))
           (end-of-prg () (= (* 2 pc) (1+ (length prg))))
           (push-output (n)
             (setf output (concatenate 'string output (when output ",") (write-to-string n)))))
      (loop while (not (or (end-of-prg) (funcall abort-if output))) do
        (let* ((inst (next-inst))
               (literal-operand (next-inst))
               (combo-operand (case literal-operand
                                ((0 1 2 3) literal-operand)
                                (4 a)
                                (5 b)
                                (6 c))))
          (ecase inst
            (0 (setf a (floor a (ash 1 combo-operand))))
            (1 (setf b (logxor b literal-operand)))
            (2 (setf b (mod combo-operand 8)))
            (3 (unless (= a 0) (setf pc literal-operand)))
            (4 (setf b (logxor b c)))
            (5 (push-output (mod combo-operand 8)))
            (6 (setf b (floor a (ash 1 combo-operand))))
            (7 (setf c (floor a (ash 1 combo-operand))))))))
    ;;(unless (funcall abort-if output) (format t "a=~a output=~a~%" a output))
    (values output a b c)))

(defun print-program (prg)
  (terpri)
  (let ((pc 0))
    (flet ((next-inst () (digit-char-p (char prg (* 2 (1- (incf pc))))))
           (end-of-prg () (= (* 2 pc) (1+ (length prg)))))
      (loop while (not (end-of-prg)) do
        (let* ((inst (next-inst))
               (literal-operand (next-inst))
               (combo-operand (case literal-operand
                                ((0 1 2 3) literal-operand)
                                (4 "A")
                                (5 "B")
                                (6 "C"))))
          (format t "~a: ~a~%" (- pc 2) (ecase inst
                                          (0 (format nil "A = A / 2^~a" combo-operand))
                                          (1 (format nil "B = B xor ~a" literal-operand))
                                          (2 (format nil "B = ~a % 8" combo-operand))
                                          (3 (format nil "jump to ~a unless A is 0" literal-operand))
                                          (4 (format nil "B = B xor C"))
                                          (5 (format nil "output ~a % 8" combo-operand))
                                          (6 (format nil "B = A / 2^~a" combo-operand))
                                          (7 (format nil "C = A / 2^~a" combo-operand)))))))))


;; (multiple-value-bind (out a b c) (program-output "2,6" :c 9)
;;   (declare (ignore out) (ignore a) (ignore c))
;;   (assert (= b 1)))

;; (assert (string= (program-output "5,0,5,1,5,4" :a 10) "0,1,2"))

;; (multiple-value-bind (out a b c) (program-output "0,1,5,4,3,0" :a 2024)
;;   (declare (ignore b) (ignore c))
;;   (assert (string= out "4,2,5,6,7,7,7,7,3,1,0"))
;;   (assert (= a 0)))

;; (multiple-value-bind (out a b c) (program-output "1,7" :b 29)
;;   (declare (ignore out) (ignore a) (ignore c))
;;   (assert (= b 26)))

;; (multiple-value-bind (out a b c) (program-output "4,0" :b 2024 :c 43690)
;;   (declare (ignore out) (ignore a) (ignore c))
;;   (assert (= b 44354)))

;;(assert (string= (program-output "0,1,5,4,3,0" :a 729) "4,6,3,5,6,3,5,2,1,0"))

(defparameter *input-data*  "2,4,1,3,7,5,0,3,1,4,4,7,5,5,3,0")
(defparameter *input-prog* '(2 4 1 3 7 5 0 3 1 4 4 7 5 5 3 0))
(print (program-output *input-data* :a 50230824))

(defun find-identity-input (prg)
  (flet ((not-matching-prg (output)
           (when output (not (string= output (subseq prg 0 (length output)))))))
    (let ((a 0))
      (loop while (not (string= prg (program-output prg :a a :abort-if #'not-matching-prg))) do
        ;;(format t "~a.. " a)
        (incf a))
      a)))

(assert (= (find-identity-input "0,3,5,4,3,0") 117440))

;;(print-program "0,3,5,4,3,0")
;;(print-program *input-data*)

(defun test-output-and-a (a)
  ;; A = A / 2^3
  ;; output A % 8
  (setf a (ash a -3))
  (values (mod a 8) a))

(defun test-prog (a)
  ;; jump to 0 unless A is 0
  (loop while (not (= a 0)) collect
                            (multiple-value-bind (output new-a) (test-output-and-a a)
                              (setf a new-a)
                              output)))

;; (defun find-a-for-test-prog ()
;;   (let ((a 1)
;;         (prg '(0 3 5 4 3 0)))
;;     ((
;;   (loop while (not (= a 0)) collect
;;                             (progn (setf a (ash a -3))
;;                                    (mod a 8))))

;;(print (test-prog 117440))
(assert (equal (test-prog 117440) '(0 3 5 4 3 0)))

(defun output (a) (mod (logxor (logxor (mod a 8) 3) 4 (ash a (- (logxor (mod a 8) 3)))) 8))

(defun a+1 (a) (ash a -3))

(defun output-and-a (a)
  ;; B = A % 8
  ;; B = B xor 3
  ;; C = A / 2^B
  ;; A = A / 2^3
  ;; B = B xor 4
  ;; B = B xor C
  ;; output B % 8
  (values (output a) (a+1 a)))

(defun list-to-int (list)
  (parse-integer (format nil "~{~a~}" list)))

(defun input-prog (a)
  ;; jump to 0 unless A is 0
  (loop while (not (= a 0)) collect
                            (multiple-value-bind (output new-a) (output-and-a a)
                              (setf a new-a)
                              output)))

(defun input-prog-as-number (a)
  (list-to-int (input-prog a)))

(compile 'input-prog)

(assert (equal (input-prog 50230824) '(2 1 4 7 6 0 3 1 4)))

(defun bisect (good bad pred)
  (when (< good bad)
    (let ((candidate (floor (+ good bad) 2)))
      (format t "Trying ~a~%" candidate)
      (cond ((= candidate good) candidate)
            ((funcall pred candidate) (progn (format t "=> good~%") (bisect candidate bad pred)))
            (t (progn (format t "=> bad~%") (bisect good candidate pred)))))))

(assert (= 4 (bisect 0 10 (lambda (x) (< x 5)))))

(let* ((min-a (1+ (bisect 0 (ash 1 100)
                          (lambda (a) (< (length (input-prog a)) (length *input-prog*))))))
       (max-a (bisect min-a (ash 1 100)
                      (lambda (a) (<= (length (input-prog a)) (length *input-prog*))))))
  (print (input-prog (1- min-a)))
  (print min-a)
  (print (input-prog min-a))
  (print max-a)
  (print (input-prog max-a))
  (print (input-prog (1+ max-a)))
  (print (1+ (- max-a min-a))))

(let ((expected-output (list-to-int *input-prog*))
      (min-cand 0))
  (let ((a (bisect min-cand (ash 1 100) (lambda (a) (<= (input-prog-as-number a) expected-output)))))
    (format t "a=~a~%" a)
    (print *input-prog*)
    (print (input-prog a))
    (print (input-prog (1+ a)))))


;; (defun find-root (guess pred improve-guess)
;;   (format t "~a: ~a - ~a = ~a~%" guess (length (input-prog guess)) (length *input-data*)
;;           (funcall pred guess))
;;   (sleep 1)
;;   (if (= (funcall pred guess) 0)
;;       guess
;;       (find-root (funcall improve-guess guess) pred improve-guess)))

;; (print (find-root 0
;;                   (lambda (a) (- (length (input-prog a)) (length *input-data*)))
;;                   (lambda (a)


;;                     floor (+ (length (input-prog a)) (length *input-data*)) 2))))

;; (defun find-min-solution (candidate pred)
;;   (if (funcall pred candidate)
;;       candidate
;;       (let ((bad candidate)
;;             (good?
;;               (if (
;;                    (let ((res-min (funcall monotonic-inc-func min))
;;                          (res-max (funcall monotonic-inc-func max)))
;;                      (when (< res-min result) (setf res-min (ceiling (+ res-min result) 2)))
;;                      (when (> res-max result) (setf res-max (floor (+ res-max result) 2)))
;;                      (setf (
;;                             (
;;                              (print (find-monotonic-int (lambda (
;;                                                             ;;(print (find-identity-input *input-data*))

;;                                                             ;;(defun find-identity-hard ()
