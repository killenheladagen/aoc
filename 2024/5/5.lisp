(require 'cl-ppcre)

(defun read-rules-and-updates-file (f)
  (let ((rules)
        (updates))
    (mapcar (lambda (line)
              (let ((new-rule (cl-ppcre:split "\\|" line))
                    (new-update  (cl-ppcre:split "," line)))
                (if (= (length new-rule) 2)
                    (push (mapcar #'parse-integer new-rule) rules)
                    (when new-update (push (mapcar #'parse-integer new-update) updates)))))
            (uiop:read-file-lines f))
    (values rules updates)))

(defun middle-item (list)
  (nth (/ (1- (length list)) 2) list))

(defun indices (list)
  (loop for i from 0 to (1- (length list)) collect i))

(defun make-index-lookup-table (list)
  (let ((lookup-table (make-hash-table :size (length list))))
    (mapc (lambda (p i)
            (setf (gethash p lookup-table) i))
          list (indices list))
    lookup-table))

(defun build-incorrect-p (rules)
  (lambda (update)
    (let ((index-table (make-index-lookup-table update)))
      (remove-if #'null
                 (mapcar
                  (lambda (rule)
                    (let ((a (gethash (car rule) index-table))
                          (b (gethash (cadr rule) index-table)))
                      (when (and a b (> a b))
                        (cons a b))))
                  rules)))))

(defun build-correct-p (rules)
  (let ((incorrect-p (build-incorrect-p rules)))
    (lambda (update)
      (null (funcall incorrect-p update)))))

(defun correct-updates (f)
  (multiple-value-bind (rules updates)
      (read-rules-and-updates-file f)
    (let ((correct-p (build-correct-p rules)))
      (remove-if-not correct-p updates))))

(defun sum-of-middle-of-correct-updates (f)
  (reduce #'+ (mapcar #'middle-item (correct-updates f))))

(assert (= (sum-of-middle-of-correct-updates "test.txt") 143))
(print (sum-of-middle-of-correct-updates "input.txt"))

(defun incorrect-updates (f)
  (multiple-value-bind (rules updates)
      (read-rules-and-updates-file f)
    (let ((incorrect-p (build-incorrect-p rules)))
      (values (remove-if-not incorrect-p updates) incorrect-p rules))))

(defun swap (i j list)
  (let ((a (nth i list))
        (b (nth j list)))
    (setf (nth i list) b)
    (setf (nth j list) a))
  list)

(defun adjust-incorrect-update (update incorrect-p)
  (let ((fix (funcall incorrect-p update)))
    (assert fix)
    (swap (caar fix) (cdar fix) update)))

(defun sum-of-middle-of-corrected-updates (f)
  (multiple-value-bind (needs-fixing incorrect-p rules)
      (incorrect-updates f)
    (labels ((fix-incorrect-update (update)
               (if (not (funcall incorrect-p update))
                   update
                   (fix-incorrect-update (adjust-incorrect-update update incorrect-p)))))
      (reduce #'+ (mapcar #'middle-item (mapcar #'fix-incorrect-update needs-fixing))))))

(assert (= (sum-of-middle-of-corrected-updates "test.txt") 123))
(print (sum-of-middle-of-corrected-updates "input.txt"))
