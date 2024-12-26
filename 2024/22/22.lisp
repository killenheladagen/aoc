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
(assert (= (print (sum-of-nth-secrets 2000 "input.txt")) 14119253575))
