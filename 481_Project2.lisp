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

" NOT NEEDED
(defun getk ()
  Get a random integer, variable, or operator
  (setq rand_num (random 3))
  (cond
    ((= rand_num 0) (setq vk (geti)))
    ((= rand_num 1) (setq vk (getv)))
	((= rand_num 2) (setq vk (make_expr)))))
"

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
	(cond ((= n 1)
		(setq n (+ 1 n))))
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
	

(defun evaluate(expr)
	"Evaluate the expression"
	(setq X *xvalue*)
	(setq Y *yvalue*)
	(setq Z *zvalue*)
	(setq result (eval expr)))

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

