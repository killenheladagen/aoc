(defun aoc (&rest func-and-results)
  (loop for (f res) on func-and-results by #'cddr do
    (assert (= (funcall f "test.txt") res))
    (print (funcall f "input.txt"))))
