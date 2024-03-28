;; Suppose we wanted a MATCH form that worked on patterns like in Rust
"let message = match x {
    0 | 1  => 0,
    2 ..= 9 => 1,
    _      => 2
};"

(defun predicate-match (l r input output)
  "Comparison predicate forms like '(= 5) or '(> 5)"
  (let* ((f (first l))
         (a (second l)))
    (if (funcall f input a)
        r
        output)))
  
(defun match (input patterns)
  (let ((output nil))
    (loop for p in patterns do
      (let ((l (first p))
	    (r (second p)))
	;; Process REFAL pattern rewrite form
	;; on match of left hand pattern
	;; rewrite into right hand output
	(cond
	  ((not l) (setq output r)) ;; default empty pattern case
	  ((numberp l) (if (eq input l)
			   (setq output r)))
	  ((listp l) (setq output
			   (predicate-match l r input output)))
	  
	  ))
	  finally (return output))

(let ((n 3))
  (match n
    '((0 0)      
      ((>= 1) 1))    
    ))
    

