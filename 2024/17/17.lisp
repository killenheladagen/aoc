
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

(defun test-prog (a)
  ;; 0: A = A / 2^3
  ;; 2: output A % 8
  ;; 4: jump to 0 unless A is 0
  (loop while (not (= a 0)) collect
                            (progn (setf a (ash a -3))
                                   (mod a 8))))

;; (defun find-a-for-test-prog ()
;;   (let ((a 1)
;;         (prg '(0 3 5 4 3 0)))
;;     ((
;;   (loop while (not (= a 0)) collect
;;                             (progn (setf a (ash a -3))
;;                                    (mod a 8))))

;;(print (test-prog 117440))

;; 0: B = A % 8
;; 2: B = B xor 3
;; 4: C = A / 2^B
;; 6: A = A / 2^3
;; 8: B = B xor 4
;; 10: B = B xor C
;; 12: output B % 8
;; 14: jump to 0 unless A is 0

(defun output-and-a (a)
  (let* ((b (logxor (mod a 8) 3))
         (c (ash a (- b))))
    (setf a (ash a -3))
    (values (mod (logxor b 4 c) 8) a)))

(defun input-prog (a)
  (loop while (not (= a 0)) collect
                            (multiple-value-bind (output new-a) (output-and-a a)
                              (setf a new-a)
                              output)))

(compile 'input-prog)

(assert (equal (input-prog 50230824) '(2 1 4 7 6 0 3 1 4)))
(print (input-prog 50230824))

;;(print (find-identity-input *input-data*))
