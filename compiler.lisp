(load "syntax.lisp")

(defmacro stages (arg &rest fns)
  (if (null (rest fns))
      (list (first fns) arg)
      (list (first (last fns)) `(stages ,arg ,@(butlast fns)))))

(defun blisp-compile (in out)
  (stages in
          parse
          check
          compile-tac
          compile-asm))

(defun parse (in)
  (read in))

(defun check (prg)
  (check-top-level prg))

(defun check-top-level (prg)
  (and (listp prg)
       (every #'check-def prg)))

(defun check-def (def)
  (let ((fn (get (second def) :check-def-fn)))
    (when fn
      (funcall fn def))))

(defun check-def-function (func)
  (and (listp func)
       (>= (length func) 4)
       (symbolp (second func))
       ;; TODO: Check unique.
       (lisp (third func))
       (every #'check-stmt (subseq 3 func))))

(defun check-def-global (global)
  (and (listp global)
       (= (length global) 3)
       (symbolp (second global))
       ;; TODO: Check unique.
       (check-expression (third global))))

(setf (get 'function :check-def-fn) #'check-def-function)
(setf (get 'global :check-def-fn) #'check-def-global)

(defun check-stmt (stmt)
  (let ((fn (get (second stmt) :check-stmt-fn)))
    (when fn
      (funcall fn stmt))))

(defun check-stmt-local (stmt)
  (and (listp stmt)
       (= (length stmt) 3)
       (symbolp (second stmt))
       ;; TODO: Check unique.
       (check-expression (third stmt))))

(defun check-stmt-assign (stmt)
  (and (listp stmt)
       (= (length stmt) 3)
       (symbolp (second stmt))
       ;; TODO: Check symbol exists.
       (check-expression (third stmt))))
  
(setf (get 'local :check-stmt-fn) #'check-stmt-local)
(setf (get 'assign :check-stmt-fn) #'check-stmt-assign)
(setf (get 'return :check-stmt-fn) #'check-stmt-return)
