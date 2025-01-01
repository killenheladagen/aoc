(require 'uiop)
(load "hash-table")
(load "dot.lisp")

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
  "Expects EXPR to contain upper-case operator and lower-case wires. Returns wires in alphabetically."
  (let ((tokens (sort (cl-ppcre:split " " expr) #'string<)))
    (values (car tokens) (cadr tokens) (caddr tokens))))

(defun call-operator (operator a b)
  (funcall (cond ((string= operator "AND") #'logand)
                 ((string= operator "OR") #'logior)
                 ((string= operator "XOR") #'logxor))
           a b))

(defun input-wires (expr)
  (multiple-value-bind (op a b) (parse-expression expr)
    (declare (ignore op))
    (sort (list a b) #'string<)))

(defun eval-symbol (sym lut)
  (let ((expr (gethash sym lut)))
    (unless (numberp expr)
      (multiple-value-bind (operator wire-a wire-b) (parse-expression expr)
        (setf expr (call-operator operator
                                  (eval-symbol wire-a lut)
                                  (eval-symbol wire-b lut))))
      ;;(setf (gethash sym lut) expr))
      )
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
    (unless (= (+ x y) z)
      (assert (format t "~a + ~a => ~a~%" x y z)))))

;; (let ((lut (read-logic-file "input.txt")))
;;   (dotimes (n 5000)
;;     (verify-add n 8 lut)))
;; 1023+1
;; 1016+8

;; (dotimes (n 45)
;;   (let* ((lut (read-logic-file "manual.txt"))
;;          (term (ash 1 (1+ n))))
;;     (verify-add (1- term) (1- term) lut)
;;     (verify-add term (1- term) lut)
;;     (verify-add (1- term) term lut)
;;     (verify-add term term lut)))


(defun split-1-2 (s)
  (values (char s 0) (subseq s 1)))

(defun parse-and-split-expression (expr)
  (multiple-value-bind (op a b) (parse-expression expr)
    (multiple-value-bind (a0 an) (split-1-2 a)
      (multiple-value-bind (b0 bn) (split-1-2 b)
        (values op a0 an b0 bn)))))

(defun is-x-or-y (c)
  (member c '(#\x #\y)))


(defun x-op-y (op c expr)
  (unless (numberp expr)
    (multiple-value-bind (expr-op a0 an b0 bn) (parse-and-split-expression expr)
      (when (and (equal expr-op op)
                 (or (is-x-or-y a0) (is-x-or-y b0)))
        (if (and (eq a0 #\x)
                 (eq b0 #\y)
                 (equal an bn))
            (when *verbose* (format t "~a OK~%" expr))
            (assert (format t "Expected xnn ~a ynn -> ~ann: ~a~%" op c expr)))
        (format nil "~a~a" c an)))))

(defun find-car (car-item list test)
  (find-if (lambda (x) (funcall test (car x) car-item)) list))

;; c01 AND a01 -> d01
(defun c-and-a (expr an-map)
  (unless (or (numberp expr) (x-op-y "AND" #\e expr))
    (multiple-value-bind (expr-op a b) (parse-expression expr)
      (when (equal expr-op "AND")
        (let ((a-role (find-car a an-map #'string=))
              (b-role (find-car b an-map #'string=)))
          (cond ((and a-role b-role) (assert (format t "~a: Match for both ~a and ~a~%"
                                                     expr a-role b-role)))
                (a-role (cons b (format nil "c~a" (subseq (cdr a-role) 1))))
                (b-role (cons a (format nil "c~a" (subseq (cdr b-role) 1))))
                (t (identity (format t "~a: Match for none of the operands~%" expr)))))))))

(defun xy-sym (op c kv)
  (let ((sym (x-op-y op c (cdr kv))))
    (when sym (cons (car kv) sym))))

;; x01 XOR y01 -> a01  "an-map"
(defun an-sym (kv) (xy-sym "XOR" #\a kv))

;; x01 AND y01 -> e01  "en-map"
(defun en-sym (kv) (xy-sym "AND" #\e kv))

;; c01 XOR a01 -> z01
(defun cn-sym (kv)
  (let ((expr (cdr kv)))
    (when (and (not (numberp expr)) (eq (char (car kv) 0) #\z))
      (multiple-value-bind (expr-op a0 an b0 bn) (parse-and-split-expression expr)
        (when (and (equal expr-op "XOR") (eq a0 #\a))
          (cons (format nil "~a~a" b0 bn) (format nil "c~a" an)))))))

;; d01 OR e01 -> c02
(defun dn-sym (kv)
  (let ((expr (cdr kv)))
    (when (and (not (numberp expr)) (eq (char (car kv) 0) #\c))
      (multiple-value-bind (expr-op a0 an b0 bn) (parse-and-split-expression expr)
        (when (and (equal expr-op "OR") (eq a0 #\e))
          (cons (format nil "~a~a" b0 bn) (format nil "d~a" an)))))))

(defparameter *verbose* nil)

(defun expr-to-edges (result expr)
  (unless (numberp expr)
    (multiple-value-bind (op a b) (parse-expression expr)
      (declare (ignore op))
      ;;(list (cons a result) (cons b result)))))
      (list (cons a expr) (cons b expr) (cons expr result)))))

(defun draw-gates (kv)
  (write-dot-file "gates.dot"
                  (mapcan #'expr-to-edges (mapcar #'car kv) (mapcar #'cdr kv)))
  (uiop:run-program (list "dot" "-Tpdf" "gates.dot" "-o" "gates.pdf")))

(defun extract-dependencies (nodes lut-kv)
  (remove-duplicates
   (mapcan (lambda (node)
             (let ((x (find-car node lut-kv #'string=)))
               (when (and x (not (numberp (cdr x))))
                 (multiple-value-bind (op a b) (parse-expression (cdr x))
                   (declare (ignore op))
                   (append (list x)
                           (extract-dependencies (list a b) lut-kv))))))
           nodes)
   :test #'equal))

(defun replace-wires (replacements lut-kv)
  (mapcar (lambda (node)
            (mapc (lambda (repl)
                    (setf node (cons (cl-ppcre:regex-replace-all (car repl) (car node) (cdr repl))
                                     (cl-ppcre:regex-replace-all (car repl) (cdr node) (cdr repl)))))
                  replacements)
            node)
          lut-kv))

(defun sort-terms (lut-kv)
  (mapcar (lambda (node)
            (cons (car node)
                  (multiple-value-bind (op a b) (parse-expression (cdr node))
                    (format nil "~a ~a ~a" a op b))))
          lut-kv))

(defun cdr-numberp (x)
  (numberp (cdr x)))


(defun format-sym (c n)
  (format nil "~a~2,'0d" c n))

;; (defun assert-expr (n res p a op b lut-kv)
;;   (let* ((sym (format-sym res (+ n p)))
;;          (expr (format nil "~a ~a ~a" (format-sym a n) op (format-sym b n)))
;;          (node (find-car sym lut-kv #'equal)))
;;     (format t "~a = ~a ? ~a~%" sym expr node)
;;     (unless node (assert (format t "~a was not found~%" sym)))
;;     (unless (equal expr (cdr node)) (assert (format t "Expected ~a to be ~a but found ~a~%"
;;                                                     sym expr (cdr node)))))
;;   nil)

;; (defun assert-full-adder (n lut-kv)
;;   (when (> n 0) (assert-expr n #\z 0 #\a "XOR" #\c lut-kv))
;;   (when (> n 1) (assert-expr n #\c 1 #\d "OR"  #\e lut-kv))
;;   (when (> n 0) (assert-expr n #\d 0 #\a "AND" #\c lut-kv))
;;   (when (> n 0) (assert-expr n #\e 0 #\x "AND" #\y lut-kv))
;;   (assert-expr n #\a 0 #\x "XOR" #\y lut-kv))

;; (defun find-full-adder-errors (filename)
;;   (multiple-value-bind (lut max-z) (read-logic-file filename)
;;     (let* ((lut-kv (remove-if #'cdr-numberp (hash-keys-and-values lut)))
;;            (an-map (remove-if #'null (mapcar #'an-sym lut-kv)))
;;            (en-map (remove-if #'null (mapcar #'en-sym lut-kv))))
;;       (mapcar (lambda (kv)
;;                 (c-and-a (cdr kv) an-map))
;;               lut-kv)
;;       ;;(print lut-kv)
;;       (setf lut-kv (replace-wires (append an-map en-map) lut-kv))
;;       (let ((cn-map (remove-if #'null (mapcar #'cn-sym lut-kv))))
;;         (setf lut-kv (replace-wires cn-map lut-kv))
;;         (let ((dn-map (remove-if #'null (mapcar #'dn-sym lut-kv))))
;;           (setf lut-kv (sort-terms (replace-wires dn-map lut-kv)))
;;           ;;(print lut-kv)
;;           (draw-gates (extract-dependencies (list "z03" "z02" "z01" "z00") lut-kv))

;;           (print (length lut-kv))
;;           (setf lut-kv (sort-terms (replace-wires '(("bbk" . "d05")
;;                                                     ("dkd" . "d07")
;;                                                     ("dkk" . "d08")
;;                                                     ("mvs" . "c10")
;;                                                     ("vbs" . "d09"))
;;                                                   lut-kv)))


;;           ;; (dotimes (n max-z)
;;           ;;   (assert-full-adder n lut-kv))

;;           nil)))))

;; (terpri)
;; (print (find-full-adder-errors "manual.txt"))

(with-open-file (s "adder.txt" :direction :output :if-exists :supersede)
  (format s "x00 XOR y00 -> z00~%")
  (format s "x00 AND y00 -> c01~%")
  (dotimes (n 43)
    (format s "x~2,'0d XOR y~2,'0d -> a~2,'0d~%" (1+ n) (1+ n) (1+ n))
    (format s "a~2,'0d XOR c~2,'0d -> z~2,'0d~%" (1+ n) (1+ n) (1+ n))
    (format s "a~2,'0d AND c~2,'0d -> d~2,'0d~%" (1+ n) (1+ n) (1+ n))
    (format s "x~2,'0d AND y~2,'0d -> e~2,'0d~%" (1+ n) (1+ n) (1+ n))
    (format s "d~2,'0d OR e~2,'0d -> c~2,'0d~%" (1+ n) (1+ n) (+ 2 n)))
  (format s "x44 XOR y44 -> a44~%")
  (format s "a44 XOR c44 -> z44~%")
  (format s "a44 AND c44 -> d44~%")
  (format s "x44 AND y44 -> e44~%")
  (format s "d44 OR e44 -> z45~%"))
;;
;; 44*5+2 = 222


(defun read-gates-from-file (filename)
  (let ((gates))
    (mapc (lambda (row)
            (cl-ppcre:register-groups-bind (a op b out) ("(.+) (.+) (.+) -> (.+)" row)
              (let ((ab (sort (list a b) #'string<)))
                (push (cons out (format nil "~a ~a ~a" (car ab) op (cadr ab))) gates))))
          (uiop:read-file-lines filename))
    gates))

(defun make-gate-hash-table (gate-list)
  (let ((ht (make-hash-table :test #'equal)))
    (mapc (lambda (gate)
            (setf (gethash (car gate) ht) (cdr gate)))
          gate-list)
    ht))

(defun merge-xors (ht)
  (let* ((first-outputs (mapcan (lambda (kv)
                                  (when (cl-ppcre:scan "x.. (XOR|AND) y.." (cdr kv))
                                    (list (car kv))))
                                (hash-keys-and-values ht)))
         ;;(format t "~a~%" kv)))
         ;;(print first-outputs)
         (new-gates (mapcan (lambda (wire)
                              (mapcan (lambda (kv)
                                        (when (cl-ppcre:scan wire (cdr kv))
                                          (list (cons (car kv)
                                                      (cl-ppcre:regex-replace wire (cdr kv)
                                                                              (format nil "(~a)"
                                                                                      (gethash wire ht)))))))
                                      (hash-keys-and-values ht)))
                            first-outputs)))
    (mapc (lambda (kv)
            (setf (gethash (car kv) ht) (cdr kv)))
          new-gates))
  ht)

(defun expression-operands (expr)
  (flet ((is-operator (x)
           (or (string= x "AND") (string= x "XOR") (string= x "OR"))))
    (remove-if #'is-operator
               (cl-ppcre:split " " (cl-ppcre:regex-replace-all "[()]" expr "")))))

(defun resolve-sym (sym ht)
  (let ((repl-expr (format nil "(~a)" (gethash sym ht))))
    (mapc (lambda (key)
            (setf (gethash key ht) (cl-ppcre:regex-replace-all sym (gethash key ht) repl-expr)))
          (copy-list (hash-keys ht)))))

(defun resolve-carry (ht)
  (flet ((is-carry-expr (kv)
           (and (cl-ppcre:scan "x.. AND y.." (cdr kv))
                (cl-ppcre:scan " OR " (cdr kv)))))
    (let* ((carry-expr (mapcar #'cdr (remove-if-not #'is-carry-expr (hash-keys-and-values ht))))
           (to-resolve (mapcan (lambda (expr)
                                 (remove-if (lambda (x) (or (eq (char x 0) #\x) (eq (char x 0) #\y)))
                                            (expression-operands expr)))
                               carry-expr)))
      (mapc (lambda (sym)
              (resolve-sym sym ht))
            to-resolve)))
  ht)

(defun extract-dependencies (nodes lut-kv)
  (remove-duplicates
   (mapcan (lambda (node)
             (let ((x (find-car node lut-kv #'string=)))
               (when (and x (not (numberp (cdr x))))
                 (append (list x)
                         (extract-dependencies (expression-operands (cdr x)) lut-kv)))))
           nodes)
   :test #'equal))

(format t "~{~a~%~}" (extract-dependencies '("z45") (hash-keys-and-values (resolve-carry (merge-xors (make-gate-hash-table (read-gates-from-file "input.txt")))))))

(defun rank (output gates)
  (labels ((f (output)
             (let ((gate (find-car output gates #'equal)))
               (if gate (reduce #'+ (mapcar #'f (subseq gate 2 4))) 1))))
    (f output)))

(defun gate-trace (output gates)
  (labels ((f (output)
             (let ((gate (find-car output gates #'equal)))
               (when gate (cons (cadr gate) (mapcan #'f (subseq gate 2 4)))))))
    (let ((trace (sort (f output) #'string<)))
      (mapcar (lambda (x) (count x trace :test #'equal)) '("AND" "OR" "XOR")))))

(defun sort-gates (gates)
  (sort (copy-list gates) (lambda (a b)
                            (let ((ra (rank (car a) gates))
                                  (rb (rank (car b) gates)))
                              (or (< ra rb)
                                  (and (= ra rb)
                                       (string< (car a) (car b))))))))

(defun print-gates (gates)
  (mapc (lambda (gate)
          (format t "~4d: ~a ~3a ~a -> ~a ~a~%"
                  (rank (car gate) gates)
                  (nth 2 gate)
                  (nth 1 gate)
                  (nth 3 gate)
                  (nth 0 gate)
                  (nth 4 gate)))
        gates))

(defun decorate (gates)
  (mapcar (lambda (gate)
            (append gate (list (gate-trace (car gate) gates))))
          (copy-list gates)))


;; (print-gates (sort-gates (read-gates-from-file "adder.txt")))
;; ;;(print-gates (sort-gates (read-gates-from-file "input.txt")))

;; (let* ((adder (decorate (read-gates-from-file "adder.txt")))
;;        (input (decorate (read-gates-from-file "input.txt"))))
;;   (print-gates (sort-gates adder))
;;   (print-gates (sort-gates input)))
