(defvar *rvars* (list 1 2 3))

(defun geti ()
  (setq const_int (- (random 19) 9)))

(defun getv ()
  (setq rand_num (random 3))
  (cond
    ((= rand_num 0) (setq v 'x))
    ((= rand_num 1) (setq v 'y))
    ((= rand_num 2) (setq v 'z))))

(defun getk ()
  (setq rand_num (random 3))
  (cond
    ((= rand_num 0) (setq vk (geti)))
    ((= rand_num 1) (setq vk (getv)))
    ((= rand_num 2) (setq vk (make_expr)))))

(defun geto ()
  (setq rand_num (random 3))
  (cond
    ((= rand_num 0) (setq op '*))
    ((= rand_num 1) (setq op '+))
    ((= rand_num 2) (setq op '-))))

(defun make_expr ()
  (Setq expr (list (geto)))
  (print expr)
  (dotimes (n (+  (random 2) 2))
    (setq expr (append expr (list (getk)))))
    (return-from make_expr expr))

(defun calc (cexpr)
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

(defun evaluate(expr)
  "Evaluate the expression"
  (setq X *x*)
  (setq Y *y*)
  (setq Z *z*)
  (setq result (eval expr)))

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
