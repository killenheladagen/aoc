(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-keys-and-values (hash-table)
  (mapcar (lambda (key) (cons key (gethash key hash-table))) (hash-keys hash-table)))
