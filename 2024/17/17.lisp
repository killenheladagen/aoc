
(defun program-output (prg &key (a 0) (b 0) (c 0))
  (let ((pc 0)
        (output))
    (flet ((next-inst () (digit-char-p (char prg (* 2 (1- (incf pc))))))
           (end-of-prg () (= (* 2 pc) (1+ (length prg))))
           (push-output (n)
             (setf output (concatenate 'string output (when output ",") (write-to-string n)))))
      (loop while (not (end-of-prg)) do
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
    (values output a b c)))

(multiple-value-bind (out a b c) (program-output "2,6" :c 9)
  (declare (ignore out) (ignore a) (ignore c))
  (assert (= b 1)))

(assert (string= (program-output "5,0,5,1,5,4" :a 10) "0,1,2"))

(multiple-value-bind (out a b c) (program-output "0,1,5,4,3,0" :a 2024)
  (declare (ignore b) (ignore c))
  (assert (string= out "4,2,5,6,7,7,7,7,3,1,0"))
  (assert (= a 0)))

(multiple-value-bind (out a b c) (program-output "1,7" :b 29)
  (declare (ignore out) (ignore a) (ignore c))
  (assert (= b 26)))

(multiple-value-bind (out a b c) (program-output "4,0" :b 2024 :c 43690)
  (declare (ignore out) (ignore a) (ignore c))
  (assert (= b 44354)))

(assert (string= (program-output "0,1,5,4,3,0" :a 729) "4,6,3,5,6,3,5,2,1,0"))

(print (program-output "2,4,1,3,7,5,0,3,1,4,4,7,5,5,3,0" :a 50230824))
