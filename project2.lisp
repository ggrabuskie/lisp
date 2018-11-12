(defun dontcalc (cexpr)
  (if (every #'not-list cexpr)
      (progn
       (apply (car cexpr) (cdr cexpr))
       (print "did it")
       (return-from calc (apply (car cexpr) (cdr cexpr)))))
  (print "else")
  (print "after"))  

(defun tree-walk (fn tree)
  (cond ((atom tree) (funcall fn tree))
	(t (tree-walk fn (first tree))
	   (tree-walk fn (rest tree)))))

(defun cell-count (rt)
  "Return the number of nodes/cells in the tree. Skip non-cells."
  (cond
    ((null rt) 0)
    ((not (listp rt)) 0)
    (t (let ((cc (length rt)))
         (+ cc (apply #'+ (mapcar #'cell-count rt)))))))

(defun not-list (mlist)
"return true if not a list"
  (not (listp mlist)))



(defun calc (rvars rexpr)
  (let ((X (car rvars))
        (Y (nth 1 rvars))
        (Z (nth 2 rvars)))
    (eval rexpr)))

(defun calc_fitness (rexpr)
  (setq tvals (list
               (list 0 -2 1 -16)
               (list -4 -5 -3 58)
               (list 9 8 -6 72)
               (list 9 -7 5 113)
               (list -8 7 3 150)
               (list 5 4 -5 20)
               (list 6 -4 6 41)
               (list -5 3 -7 -24)
               (list -6 -5 9 -18)
               (list 1 0 2 2)))
  (reduce '+ (loop for i in tvals
        for sl = (abs(-  (calc i rexpr) (nth 3 i)))
                   collect sl)))


(defun get_population (size)
  (loop for i from 1 to size
        for expr = (make_expr)
        collect expr))

(defun collect-fits (population)
  (loop for i in population
        for fits = (calc_fitness i)
        collect fits))

(defun smallest (l)
  "from https://www.programcreek.com/2010/10/lisp-code-find-minimum-element-in-a-list/"
  (apply 'min l))

(defun get_best_fit (population)
  (smallest (collect-fits population)))

(defun main ()
  (setq population (get_population 50)))

