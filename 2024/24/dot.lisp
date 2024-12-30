(require 'cl-ppcre)

(defun dot-name (name)
  (cl-ppcre:regex-replace-all " " name "_"))

(defun write-dot-file (filename edges)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "digraph {~%concentrate=true~%~{~a~%~}~%~{~a~%~}~%}"
	    (mapcar (lambda (node)
		      (format nil "~a" (dot-name node)))
		    (remove-duplicates (mapcan (lambda (x) (list (car x) (cdr x))) edges) :test #'equal))
	    (mapcar (lambda (edge) (format nil "~a->~a" (dot-name (car edge)) (dot-name (cdr edge))))
                    edges))))
