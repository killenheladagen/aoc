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
  (mapcar (lambda (tail-path)
            (cons start tail-path))
          (flet ((is-wall-or-outside (pos)
                   (case (at pos b) ((#\# nil) t))))
            (let ((next (remove-if #'is-wall-or-outside
                                   (next-pos-towards-dest start end))))
              (paths-to-pos

               ;;            (build-paths (from to-path)
               ;;              (if (eq from (car to-path))
               ;;                  (list to-path)
               ;;                  (mapcan (lambda (pos)
               ;;                            (build-paths from (cons pos to-path)))
               ;;                          (remove-if #'is-wall-or-outside
               ;;                                     (last-pos-towards-dest from (car to-path)))))))
               ;;     (build-paths start (list end))))

               ;;(defun path-to-pos (start end b)
               ;;  (car (paths-to-pos start end b))) ;; Since we always end at A, finding multiple paths was pointless

               (defun path-to-pos (start end b)
                 (cons start
                       (unless (= start end)
                         (flet ((is-wall-or-outside (pos)
                                  (case (at pos b) ((#\# nil) t))))
                           (let ((next (car (remove-if #'is-wall-or-outside
                                                       (next-pos-towards-dest start end)))))
                             (path-to-pos next end b))))))

               (defun path-steps (path)
                 (mapcar #'- (cdr path) path))

               (defun shortest-path-step-string (path)
                 (coerce (append (sort (mapcar #'step-char (path-steps path)) #'char>)
                                 (list #\A))
                         'string))

               (defun path-through-symbols (symbols b &optional start-pos)
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
               (defun shortest-sequence (code)
                 (labels ((f (symbols board-funcs)
                            (if board-funcs
                                (f (path-through-symbols symbols
                                                         (funcall (car board-funcs)))
                                   (cdr board-funcs))
                                symbols)))
                   (f code (list #'numeric-keypad
                                 #'directional-keypad
                                 #'directional-keypad
                                 ))))


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
