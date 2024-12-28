(require 'cl-ppcre)
(require 'uiop)

(defun read-logic-file (filename)
  (let ((lut (make-hash-table :test #'equal))
        (max-z 0))
    (mapc (lambda (row)
            (cl-ppcre:register-groups-bind (wire (#'parse-integer wire-value)) ("(.+): (.+)" row)
              (setf (gethash wire lut) wire-value))
            (cl-ppcre:register-groups-bind (expression wire) ("(.+) -> (.+)" row)
              (setf (gethash wire lut) expression)
              (when (eq (char wire 0) #\z)
                (setf max-z (max max-z (parse-integer (subseq wire 1)))))))
          (uiop:read-file-lines filename))
    (values lut max-z)))

(defun parse-expression (expr)
  (cl-ppcre:register-groups-bind (wire-a operator wire-b) ("(.+) (.+) (.+)" expr)
    (values (cond ((string= operator "AND") #'logand)
                  ((string= operator "OR") #'logior)
                  ((string= operator "XOR") #'logxor))
            wire-a wire-b)))

(defun input-wires (expr)
  (multiple-value-bind (op a b) (parse-expression expr)
    (declare (ignore op))
    (list a b)))

(defun eval-symbol (sym lut)
  (let ((expr (gethash sym lut)))
    (unless (numberp expr)
      (multiple-value-bind (operator wire-a wire-b) (parse-expression expr)
        (setf expr (funcall operator
                            (eval-symbol wire-a lut)
                            (eval-symbol wire-b lut))))
      (setf (gethash sym lut) expr))
    expr))

(defun eval-logic-file (filename)
  (multiple-value-bind (lut max-z) (read-logic-file filename)
    (labels ((eval-z (n)
               (+ (ash (eval-symbol (format nil "z~2,'0d" n) lut) n)
                  (if (= n 0) 0 (eval-z (1- n))))))
      (eval-z max-z))))


(assert (= (eval-logic-file "test.txt") 4))
(assert (= (eval-logic-file "test2.txt") 2024))
(assert (= (print (eval-logic-file "input.txt")) 36902370467952))

(defun get-int (prefix lut)
  (labels ((eval-z (n)
             (+ (ash (eval-symbol (format nil "~a~2,'0d" prefix n) lut) n)
                (if (= n 0) 0 (eval-z (1- n))))))
    (eval-z (if (eq prefix #\z) 45 44))))

(defun set-int (prefix value lut)
  (labels ((set-bit (n value)
             (setf (gethash (format nil "~a~2,'0d" prefix n) lut) (ash value (- n)))
             (when (> n 0)
               (set-bit (1- n) (logand value (1- (ash 1 n)))))))
    (set-bit 44 value))
  (assert (= (get-int prefix lut) value)))

(defun set-inputs (x y lut)
  (set-int #\x x lut)
  (set-int #\y y lut))

(defun get-output (lut)
  (get-int #\z lut))

(defun verify-add (x y lut)
  (set-inputs x y lut)
  (let ((z (get-output lut)))
    (format t "~a + ~a => ~a~%" x y z)
    (assert (= (+ x y) z))))

(dotimes (n 45)
  (let* ((lut (read-logic-file "manual.txt"))
         (term (ash 1 (1+ n))))
    (verify-add (1- term) (1- term) lut)
    (verify-add term (1- term) lut)
    (verify-add (1- term) term lut)
    (verify-add term term lut)))


;; x01 XOR y01 -> a01
;; c01 XOR a01 -> z01
;; c01 AND a01 -> d01
;; x01 AND y01 -> e01
;; d01 OR e01 -> c02
