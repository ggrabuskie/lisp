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
    ((= rand_num 0) (setq vori (geti)))
    ((= rand_num 1) (setq vori (getv)))
    ((= rand_num 2) (setq vori (make_expr)))))

(defun geto ()
  (setq rand_num (random 3))
  (cond

    ((= rand_num 0) (setq op '*))
    ((= rand_num 1) (setq op '+))
    ((= rand_num 2) (setq op '-))))

(defun make_expr ()
  (Setq expr (list (geto)))
  (print expr)
  (dotimes (n (random 4))
    (setq expr (append expr (list (getk)))))
    (return-from make_expr expr))
    

