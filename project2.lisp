(defvar *x* 1)
(defvar *y* 2)
(defvar *z* 3)

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
