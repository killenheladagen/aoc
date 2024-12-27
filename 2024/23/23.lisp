(require 'cl-ppcre)
(require 'uiop)

(defun read-connection-file (filename)
  (mapcar (lambda (line) (sort (cl-ppcre:split "-" line) #'string<))
          (uiop:read-file-lines filename)))

;;(defstruct node (name neighbors))

(defun build-graph (filename)
  (let ((graph (make-hash-table :test #'equal)))
    (flet ((append-neighbor (node new-neighbor)
             (push new-neighbor (gethash node graph nil))))
      (mapc (lambda (x)
              (append-neighbor (car x) (cadr x))
              (append-neighbor (cadr x) (car x)))
            (read-connection-file filename)))
    graph))

;; (defun neighbors-neighbors (node graph)
;;   (sort (remove-duplicates (remove-if (lambda (x) (equal x node))
;;                                       (mapcan (lambda (x) (gethash x graph)) (gethash node graph)))
;;                            :test #'equal)
;;         #'string<))

;; (defun neighbors-reachable-through-other-neighbors (node graph)
;;   (intersection (gethash node graph)
;;                 (neighbors-neighbors node graph)
;;                 :test #'equal))

(defun sets-of-3-for-node (node graph)
  (remove-duplicates
   (let ((directs (gethash node graph)))
     (mapcan (lambda (d)
               (let* ((indirects (remove-if (lambda (x) (equal x node)) (gethash d graph)))
                      (intercons (intersection directs indirects :test #'equal)))
                 ;;(format t "dir=~a ind(~a)=~a int=~a~%"
                 ;;        directs d indirects intercons)
                 (mapcar (lambda (x) (sort (list node d x) #'string<))
                         intercons)))
             directs))
   :test #'equal))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun sets-of-3 (filename)
  (let ((graph (build-graph filename)))
    (sort
     (remove-duplicates
      (mapcar (lambda (nodes)
                (format nil "~{~a~^,~}" nodes))
              (mapcan (lambda (node) (sets-of-3-for-node node graph))
                      (hash-keys graph)))
      :test #'equal)
     #'string<)))

(assert (equal (sets-of-3 "test.txt")
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
                    (sets-of-3 filename)))))

(assert (= (number-of-sets-of-3-starting-with-t "test.txt") 7))
(assert (= (print (number-of-sets-of-3-starting-with-t "input.txt")) 1248))
