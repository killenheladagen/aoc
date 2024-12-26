(defun next-secret (x)
  (let ((secret x))
    (labels ((mix-and-prune (a)
               (setf secret (mod (logxor a secret) 16777216)))
             (mul64 (a)
               (mix-and-prune (* a 64)))
             (div32 (a)
               (mix-and-prune (floor a 32)))
             (mul2048 (a)
               (mix-and-prune (* a 2048))))
      (mul2048 (div32 (mul64 x))))))

(defun secret-sequence (n seed)
  (when (> n 0)
    (let ((x (next-secret seed)))
      (cons x (secret-sequence (1- n) x)))))

(defun nth-secret (n seed)
  (if (<= n 0) seed (nth-secret (1- n) (next-secret seed))))

(assert (equal (secret-sequence 10 123)
               '(15887950
                 16495136
                 527345
                 704524
                 1553684
                 12683156
                 11100544
                 12249484
                 7753432
                 5908254)))

(assert (= (nth-secret 10 123) 5908254))

(defun read-integer-list-file (f)
  (mapcar #'parse-integer (uiop:read-file-lines f)))

(defun sum-of-nth-secrets (n filename)
  (reduce #'+
          (mapcar (lambda (seed)
                    (nth-secret n seed))
                  (read-integer-list-file filename))))

(assert (= (sum-of-nth-secrets 2000 "test.txt") 37327623))
;;(assert (= (print (sum-of-nth-secrets 2000 "input.txt")) 14119253575))

(defun ones-digit (x)
  (mod x 10))

(defun ones-digit-of-secrets (seed)
  (mapcar #'ones-digit (secret-sequence 2000 seed)))

(defun encode-price-and-sequence (x)
  (cons (car x) (+ (* (+ 10 (nth 1 x)) 1)
                   (* (+ 10 (nth 2 x)) 100)
                   (* (+ 10 (nth 3 x)) 10000)
                   (* (+ 10 (nth 4 x)) 1000000))))

(defun decode-price-and-sequence (x)
  (let ((y (cdr x)))
    (list (car x)
          (- (mod y 100) 10)
          (- (floor (mod y 10000) 100) 10)
          (- (floor (mod y 1000000) 10000) 10)
          (- (floor (mod y 100000000) 1000000) 10))))

(defun refine-price-list (digits)
  (let ((changes (mapcar #'- (cdr digits) digits)))
    (mapcar #'encode-price-and-sequence
            (mapcar #'list (cddddr digits) changes (cdr changes) (cddr changes) (cdddr changes)))))

(defun prune-price-list (refined-list)
  (flet ((first-seq-match (seq)
           (find-if (lambda (x) (= (cdr x) seq)) refined-list))
         (zero-price (price-and-seq)
           (zerop (car price-and-seq))))
    (let* ((unique-sequences (remove-duplicates (mapcar #'cdr refined-list))))
      (sort
       (remove-if #'zero-price (mapcar #'first-seq-match unique-sequences))
       (lambda (a b) (< (cdr a) (cdr b)))))))

(defun max-price (refined-lists)
  (let* ((combined-lists (mapcan #'identity refined-lists))
         (unique-sequences (remove-duplicates (mapcar #'cdr combined-lists))))
    (reduce #'max
            (mapcar (lambda (x)
                      (reduce #'+ (mapcar #'car (remove-if-not (lambda (y) (= x (cdr y)))
                                                               combined-lists))))
                    unique-sequences))))

(defun most-num-bananas (filename)
  (let ((prices (mapcar #'prune-price-list
                        (mapcar #'refine-price-list
                                (mapcar #'ones-digit-of-secrets
                                        (read-integer-list-file filename))))))
    (max-price prices)))

(assert (= (most-num-bananas "test2.txt") 23))
(print (most-num-bananas "input.txt"))
