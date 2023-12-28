(defun read-edges (i)
  (mapcan (lambda (line)
	    (flet ((node-sym (s) (intern (string-upcase (subseq s 0 3)))))
	      (let ((nodes (mapcar #'node-sym (uiop:split-string line))))
		(mapcar (lambda (x) (cons (car nodes) x)) (cdr nodes)))))
	  (uiop:read-file-lines (format nil "25-~d.txt" i))))

(defun make-bidir (edge-list)
  (mapcan (lambda (edge)
	    (list (cons (car edge) (cdr edge))
		  (cons (cdr edge) (car edge))))
	  edge-list))

(defun nodes (edge-list)
  (remove-duplicates (mapcan (lambda (edge) (list (car edge) (cdr edge))) edge-list)))

(defun node-and-neighbor-list (edge-list)
  (let ((bidir-edges (make-bidir edge-list)))
    (flet ((add-neighbors (node)
	     (cons node (mapcar 'cdr (remove-if-not (lambda (e) (equal node (car e))) bidir-edges)))))
      (mapcar #'add-neighbors (nodes edge-list)))))


(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) (eql (car x) node)) edge-list))

(defun get-connected (node edge-list)
  (let ((visited))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge) (traverse (cdr edge))) (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun island-product (edge-list)
  (let* ((bidir-edges (make-bidir edge-list))
	 (island-a (get-connected (caar edge-list) bidir-edges))
	 (island-b (remove-if (lambda (x) (member x island-a)) (nodes edge-list))))
    (when island-b (* (length island-a) (length island-b)))))

(defun comp-distance (edge-list unvisited)
  (when unvisited
    (let ((cur (car unvisited)))
      (flet ((update-distance (node)
	       (let* ((edge (cons (car cur) (car node)))
		      (is-neighbour (member edge edge-list :test #'equal)))
		 (cons (car node) (if is-neighbour (min (1+ (cdr cur)) (cdr node)) (cdr node))))))
	(cons cur (comp-distance edge-list
				 (sort (mapcar #'update-distance (cdr unvisited)) '< :key 'cdr)))))))

(defun djik (edge-list)
  (let* ((n (nodes edge-list))
	 (unvisited (cons (cons (car n) 0)
			  (mapcar (lambda (x) (cons x most-positive-fixnum)) (cdr n)))))
    (comp-distance (make-bidir edge-list) unvisited)))

(defun count-neighbors (edge-list)
  (sort
   (mapcar (lambda (n)
	     (cons n (count-if (lambda (e)
				 (or (equal n (car e))
				     (equal n (cdr e))))
			       edge-list)))
	   (nodes edge-list))
   '< :key 'cdr))

(defun distribute-edges (edge-list island-a island-b)
  (format t "~%A:~a~%B:~a~%EL:~a~%" island-a island-b edge-list)
  (if (<= (length edge-list) 3)
      edge-list
      (let* ((edge-for-a (find-if (lambda (edge)
				    (and
				     (or (member (car edge) island-a :test 'equal)
					 (member (cdr edge) island-a :test 'equal))
				     (not (or (member (car edge) island-b :test 'equal)
					      (member (cdr edge) island-b :test 'equal)))))
				  edge-list)))
	(if edge-for-a
	    (let* ((new-a
		     (remove-duplicates (cons (car edge-for-a) (cons (cdr edge-for-a) island-a))))
		   (fewer-edges (remove edge-for-a edge-list :test 'equal)))
	      (distribute-edges fewer-edges island-b new-a))
	    (progn
	      (distribute-edges edge-list island-b island-a))))))

(defun main (i)
  (let* ((edge-list (read-edges i))
	 (start (if (= i 0) '(rhn frs) '(xtd pfz)))
	 (edges-to-remove (distribute-edges edge-list (list (car start)) (list (cadr start))))
	 (edges-after-removal
	   (remove-if (lambda (x) (member x edges-to-remove :test 'equal)) edge-list)))
    (assert (= (+ 3 (length edges-after-removal)) (length edge-list)))
    (print edges-to-remove)
    (island-product edges-after-removal)))

(defun foo (edge-list)
  (when edge-list
    (print (length edge-list))
    (or (island-product edge-list) (foo (cdr edge-list)))))

(defun find-island-product (edge-list &optional (cuts-left 3))
  (if (zerop cuts-left)
      (island-product edge-list)
      (flet ((all-but (x) (remove x edge-list :test #'equal)))
	(let* ((combos (mapcar #'all-but edge-list))
	       (prods (mapcar (lambda (x) (find-island-product x (1- cuts-left))) combos)))
	  (remove-if #'null prods)))))


(defun format-dot (dest node-list edge-list)
  (format dest "digraph {~%concentrate=true~%~{~a~%~}~%~{~a~%~}~%}"
	  (mapcar (lambda (node)
		    (format nil "~a [label=\"~a (~a)\"]" (car node) (car node) (cdr node)))
		  node-list)
	  (mapcar (lambda (edge)
		    (format nil "~a->~a->~a"
			    (car edge) (cdr edge) (car edge)))
		  edge-list)))

;; max
(defun main ()
  (with-open-file (s "25.dot" :direction :output :if-exists :supersede)
    (let* ((edges (read-edges 1))
	   (distances (count-neighbors edges)));;(djik edges)))
      ;;(print (find-island-product edges))
      (when nil (setf edges (remove-if (lambda (x)
			       (or (equal x '(pzl . hfx))
				   (equal x '(cmg . bvb))
				   (equal x '(jqt . nvd))))
			     edges)))
      (print edges)
      (format-dot s distances edges))))

