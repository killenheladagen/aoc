(load "board")

(defun numeric-keypad ()
  (make-array '(4 3) :initial-contents '((#\7 #\8 #\9)
                                         (#\4 #\5 #\6)
                                         (#\1 #\2 #\3)
                                         (#\# #\0 #\A))))

(defun directional-keypad ()
  (make-array '(2 3) :initial-contents '((#\# #\^ #\A)
                                         (#\< #\v #\>))))

(defun start-pos (b) (find-eq #\A b))

(defun sign (x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (t 0)))

(defun cardinal-steps-towards (a)
  (remove-if #'zerop (list (sign (realpart a)) (complex 0 (sign (imagpart a))))))

(defun next-pos-towards-dest (a b)
  (mapcar (lambda (step) (+ a step)) (cardinal-steps-towards (- b a))))

(defun last-pos-towards-dest (a b)
  (mapcar (lambda (step) (- b step)) (cardinal-steps-towards (- b a))))

(defun paths-to-pos (start end b)
  (labels ((is-wall-or-outside (pos)
             (case (at pos b) ((#\# nil) t)))
           (paths-to-tail (tail)
             (mapcan (lambda (tail-start)
                       (let ((new-tail (cons tail-start tail)))
                         (if (eq (car new-tail) start)
                             (list new-tail)
                             (paths-to-tail new-tail))))
                     (remove-if #'is-wall-or-outside
                                (last-pos-towards-dest start (car tail))))))
    (paths-to-tail (list end))))

(defun paths-through-pos (pos-to-visit b)
  (print pos-to-visit)
  (print
   (when (> (length pos-to-visit) 1)
     (mapcan (lambda (first-leg)
               (print first-leg)
               (append (list first-leg) (paths-through-pos (cdr pos-to-visit) b)))
             (paths-to-pos (car pos-to-visit) (cadr pos-to-visit) b)))))

(defun path-steps (path)
  (mapcar #'- (cdr path) path))

(defun shortest-path-step-string (path)
  (coerce (append (sort (mapcar #'step-char (path-steps path)) #'char>)
                  (list #\A))
          'string))

(defun paths-through-symbols (symbols b)
  (flet ((symbol-to-pos (s) (find-eq s b)))
    (mapcar #'symbol-to-pos symbols)
    (unless (= (length symbols) 0)
      (let* ((next-char (char symbols 0))
             (next-pos (car (find-eq next-char b)))
             (from-pos (or start-pos (car (find-eq #\A b)))))
        (concatenate 'string
                     (shortest-path-step-string (path-to-pos from-pos next-pos b))
                     (path-through-symbols (subseq symbols 1) b next-pos)))))

  ;; (defun sequences (itineraries boards)
  ;;   (if boards
  ;;       (mapcar (lambda (symbols)
  ;;                 (sequences (path-through-symbols symbols (funcall (car boards))) (cdr boards)))
  ;;               itineraries)
  ;;       itineraries))

  ;; (defun shortest-sequence (code)
  ;;   (sequences (list code) (list #'numeric-keypad)))
  (defun sequences (code)
    (labels ((f (symbols board-funcs)
               (if board-funcs
                   (mapcan (lambda (s)
                             (f (paths-through-symbols s (funcall (car board-funcs)))
                                (cdr board-funcs)))
                           symbols)
                   symbols)))
      (f (list code) (list #'numeric-keypad #'directional-keypad #'directional-keypad))))

  (defun shortest-sequence (code)
    (reduce (lambda (a b) (if (<= (length a) (length b)) a b)) (sequences code)))

  (assert (= (length (shortest-sequence "029A")) 68))
  (assert (= (length (shortest-sequence "980A")) 60))
  (assert (= (length (shortest-sequence "179A")) 68))
  (assert (= (length (shortest-sequence "456A")) 64))
  (assert (= (length (shortest-sequence "379A")) 64))

  (defun sum-of-complexities (list-of-codes)
    (reduce #'+ (mapcar (lambda (code)
                          (* (length (shortest-sequence code))
                             (parse-integer code :junk-allowed t)))
                        list-of-codes)))

  ;;(assert (= (print (sum-of-complexities '("029A" "980A" "179A" "456A" "379A"))) 126384))
  ;; 123844 too low

  (print (sum-of-complexities '("341A" "803A" "149A" "683A" "208A")))
  ;; 160876 too high
