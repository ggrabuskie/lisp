(defun geti ()
  (setq const_int (- (random 19) 9)))

(defun getv ()
  (setq rand_num (random 3))
  (cond
    ((= rand_num 0) (setq v 'x))
    ((= rand_num 1) (setq v 'y))
    ((= rand_num 2) (setq v 'z))))
(defun getvori ()
  (setq rand_num (random 2))
  (cond
    ((= rand_num 0) (setq vori (geti)))
    ((= rand_num 1) (setq vori (getv)))))

(defun geto ()
  (setq rand_num (random 3))
  (cond

    ((= rand_num 0) (setq op '*))
    ((= rand_num 1) (setq op '+))
    ((= rand_num 2) (setq op '-))))
(defun make_expr ()
  (setq numOps (random 4))
  (setq expr (list (geto)))
  (print expr)
  (dotimes (n numOps)
    (print n)
    (append expr
    ))
