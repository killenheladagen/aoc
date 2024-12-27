(require 'cl-ppcre)
(require 'uiop)

(defun read-connection-file (filename)
  (mapcar (lambda (line) (sort (cl-ppcre:split "-" line) #'string<))
          (uiop:read-file-lines filename)))

(defun build-graph (filename)
  (let ((graph (make-hash-table :test #'equal)))
    (flet ((append-neighbor (node new-neighbor)
             (push new-neighbor (gethash node graph nil))))
      (mapc (lambda (x)
              (append-neighbor (car x) (cadr x))
              (append-neighbor (cadr x) (car x)))
            (read-connection-file filename)))
    graph))

(defun sets-of-3-for-node (node graph)
  (remove-duplicates
   (let ((directs (gethash node graph)))
     (mapcan (lambda (d)
               (let* ((indirects (remove-if (lambda (x) (equal x node)) (gethash d graph)))
                      (intercons (intersection directs indirects :test #'equal)))
                 (mapcar (lambda (x) (sort (list node d x) #'string<))
                         intercons)))
             directs))
   :test #'equal))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun format-nodes (nodes)
  (format nil "~{~a~^,~}" nodes))

(defun nodes< (a b)
  (string< (format-nodes a) (format-nodes b)))

(defun sets-of-3 (filename)
  (let ((graph (build-graph filename)))
    (values
     (sort
      (remove-duplicates
       (mapcan (lambda (node) (sets-of-3-for-node node graph))
               (hash-keys graph))
       :test #'equal)
      #'nodes<)
     graph)))

(defun format-sets-of-3 (filename)
  (mapcar #'format-nodes (sets-of-3 filename)))

(assert (equal (format-sets-of-3 "test.txt")
               '("aq,cg,yn"
                 "aq,vc,wq"
                 "co,de,ka"
                 "co,de,ta"
                 "co,ka,ta"
                 "de,ka,ta"
                 "kh,qp,ub"
                 "qp,td,wh"
                 "tb,vc,wq"
                 "tc,td,wh"
                 "td,wh,yn"
                 "ub,vc,wq")))

(defun number-of-sets-of-3-starting-with-t (filename)
  (length
   (let ((starts-with-t (ppcre:create-scanner "(^|,)t")))
     (remove-if-not (lambda (x) (ppcre:scan starts-with-t x))
                    (format-sets-of-3 filename)))))

(assert (= (number-of-sets-of-3-starting-with-t "test.txt") 7))
(assert (= (print (number-of-sets-of-3-starting-with-t "input.txt")) 1248))

;; (defun sets-of-interconnected-for-node (node graph)
;;       (sets
;;   (remove-duplicates
;;    (let ((directs (gethash node graph)))
;;      (mapcan (lambda (d)
;;                (let* ((indirects (remove-if (lambda (x) (equal x node)) (gethash d graph)))
;;                       (intercons (intersection directs indirects :test #'equal)))
;;                  (when intercons (list (sort (cons node (cons d intercons)) #'string<)))))
;;              directs))
;;    :test #'equal))

(defun common-neighbors (nodes graph)
  (if (= (length nodes) 1)
      (gethash (car nodes) graph)
      (intersection (gethash (car nodes) graph)
                    (common-neighbors (cdr nodes) graph)
                    :test #'equal)))

(defun longest (a b)
  (if (>= (length a) (length b)) a b))

(defun max-length (list-of-lists)
  (reduce #'max (mapcar #'length list-of-lists)))

(defun largest-set (filename)
  (multiple-value-bind (sets graph) (sets-of-3 filename)
    (labels ((append-common-neighbors (sets)
               (sort
                (remove-duplicates
                 (mapcar (lambda (nodes)
                           (sort (append nodes (common-neighbors nodes graph)) #'string<))
                         sets)
                 :test #'equal)
                #'nodes<))
             (extend-and-keep-largest (sets)
               (let* ((old-max-len (max-length sets))
                      (new-sets (append-common-neighbors sets))
                      (max-len (max-length new-sets))
                      (max-len-sets (remove-if-not (lambda (x) (= (length x) max-len))
                                                   new-sets)))
                 (if (= old-max-len max-len)
                     (car max-len-sets)
                     (extend-and-keep-largest max-len-sets)))))
      (format-nodes (extend-and-keep-largest sets)))))

(assert (string= (largest-set "test.txt") "co,de,ka,ta"))
(assert (string= (print (largest-set "input.txt")) "aa,cf,cj,cv,dr,gj,iu,jh,oy,qr,xr,xy,zb"))
