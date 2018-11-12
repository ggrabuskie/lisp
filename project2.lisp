(defvar *xvalue* 1)
(defvar *yvalue* 2)
(defvar *zvalue* 3)

(defun geti ()
  "Get a random integer"
  (setq const_int (- (random 19) 9)))

(defun getv ()
  "Get a random variable"
  (setq rand_num (random 3))
  (cond
    ((= rand_num 0) (setq v 'x))
    ((= rand_num 1) (setq v 'y))
    ((= rand_num 2) (setq v 'z))))


(defun getvi ()
  "Get a random integer or variable."
  (setq rand_num (random 2))
  (cond
    ((= rand_num 0) (setq vk (geti)))
    ((= rand_num 1) (setq vk (getv)))))

(defun geto ()
  "Get a random operator"
  (setq rand_num (random 3))
  (cond
    ((= rand_num 0) (setq op '*))
    ((= rand_num 1) (setq op '+))
    ((= rand_num 2) (setq op '-))))

(defun getRanN (expr)
  "Get a random number for n"
  (setq n (+ (random (cell-count expr)) 1))
  n)

(defun make_expr()
  "Call this function to create a random expression"
  (setq expr (first_layer))
  (add_layer expr))

(defun first_layer()
  "Creates the first layer of expression"
  (setq expr (list (geto)))
  (dotimes (n 3)
    (setq expr (append expr (list (getvi)))))
  (return-from first_layer expr))

(defun add_layer(expr)
  "Adds 2nd layer to expression in a random spot"
  (setq insert (first_layer))
  (setq n (getRanN expr))
  (if (= 1 n)
      (+ n 1))
  (loop for i from 1 for j in expr collect (if (= i n) insert j)))

(defun mutation (expr)
  "Call this function for a mutation"
  (setq rnth (getRanN expr))
  (mutate rnth expr))

(defun mutate (rnth expr)
  "Replaces one cell with a random variable or operator"
  (setq subThisop (geto))
  (setq subThisv (getvi))
  (let ((size (cell-count expr)))
    (cond
      ((not(integerp  rnth)) nil)
      ((null expr) nil)	;; No tree elts?
      ((= 1 rnth)		;; Found the correct position
       (cond
	 ((listp (car expr))
	  (print rnth)
	  (mutate (+ 1 rnth) expr))
	 ((equal 'X (car expr))
	  (rplaca expr subThisv))
	 ((equal 'Y (car expr))
	  (rplaca expr subThisv))
	 ((equal 'Z (car expr))
	  (rplaca expr subThisv))
	 ((integerp (car expr))
	  (rplaca  expr subThisv))
	 (t
	  (rplaca expr subThisop))))
      ((>= 0 rnth) nil)	;; Nth 0 or negative?
      ((> rnth size) nil)	;; N-th beyond tree's end?
      (t	;; elt is in the car subtree or cdr "subtree"
       (setq rnth (1- rnth))	;; Account: elt isn't the current (car+cdr's) node
       (let ((size1 (cell-count (car expr))))
	 (cond
	   ((>= 0 size1) (mutate ;; No car subtree
			  rnth
			  (cdr expr)))	;; Find elt in the cdr subtree
	   ((<= rnth size1) (mutate	;; Elt is in car subtree
			     rnth
			     (car expr)))	;; Find elt in the car subtree
	   (t (mutate	;; Elt is in the cdr subtree
	       (- rnth size1)	;; Accounting for skipping car subtree
	       (cdr expr)))))))))	;; skip car subtree

(defun cell-count (rt)
  "Return the number of nodes/cells in the tree. Skip non-cells."
  (cond
    ((null rt) 0)
    ((not (listp rt)) 0)
    (t (let ((cc (length rt)))
         (+ cc (apply #'+ (mapcar #'cell-count rt)))))))

(defun tree-nth-cell (rnth rtree)
  "Return the DFS N-th cell in the given tree: 1-based"
  (let ((size (cell-count rtree)))
    ;; (print (list :dbga rnth size (car rtree)))
    (cond
      ((not(integerp  rnth)) nil)
      ((not (listp rtree)) nil)	;; Not a tree?
      ((null rtree) nil)	;; No tree elts?
      ((= 1 rnth) rtree)	;; 1st elt of list is the tree, itself
      ((>= 0 rnth) nil)	;; Nth 0 or negative?
      ((> rnth size) nil)	;; N-th beyond tree's end?
      (t	;; elt is in the car subtree or cdr "subtree"
       (setq rnth (1- rnth))	;; Account: elt isn't the current (car+cdr's) node
       (let ((size1 (cell-count (car rtree))))
	 ;; (print (list :dbgb rnth size1 (car rtree)))
	 (cond
	   ((>= 0 size1) (tree-nth-cell ;; No car subtree
			  rnth
			  (cdr rtree)))	;; Find elt in the cdr subtree
	   ((<= rnth size1) (tree-nth-cell	;; Elt is in car subtree
			     rnth
			     (car rtree)))	;; Find elt in the car subtree
	   (t (tree-nth-cell	;; Elt is in the cdr subtree
	       (- rnth size1)	;; Accounting for skipping car subtree
	       (cdr rtree)))))))))	;; skip car subtree

(defun random-tree-cell (rtree)
  "Return random cell in the tree, but not the whole tree."
  (let* ((size (cell-count rtree))
	 (rx (1+ (random (1- size))))	;; Avoid 1st cell (the whole tree)
	 (nth (1+ rx))	;; Incr because our fcn is 1-based, not 0-based
	 (spot (tree-nth-cell nth rtree)))
    ;; (print (list :dbg size nth spot))
    spot ))
(defun tree-walk (fn tree)
  (cond ((atom tree) (funcall fn tree))
	(t (tree-walk fn (first tree))
	   (tree-walk fn (rest tree)))))

(defun not-list (mlist)
  "return true if not a list"
  (not (listp mlist)))

(defun calc (rvars rexpr)
  (let ((X (car rvars))
        (Y (nth 1 rvars))
        (Z (nth 2 rvars)))
    (eval rexpr)))

(defun calc-fitness (rexpr)
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


(defun get-population (size)
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

(defun get-best-fit (population)
  (smallest (collect-fits population)))

(defun add-best (bestl newbest)
  "make sure to save return value because bestl is local and will not be saved globally"
  (if (not (car bestl))
      (setq bestl (list newbest))
      (setq bestl (append bestl (list newbest)))))

(defun main (max-gens gen-size)
  (setq best-of-gens (list(list)))
  (dotimes (n max-gens)
    (setq population (get_population gen-size))
    (setq best-of-gens (add-best best-of-gens (get-best-fit population)))))

