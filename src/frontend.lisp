;;;; Compiler frontend - semantic analysis & translation to kernel IR

(in-package :strl2v)

(defun elaborate (program)
  (make-kprogram :modules (mapcar #'elaborate-module (program-modules program))))

(defun elaborate-module (module)
  (translate-module module))

(defun translate-module (module)
  (make-kmodule :name (module-def-name module)
		:inputs (module-def-inputs module)
		:outputs (module-def-outputs module)
		:body (translate-stmt (module-def-body module))))

(defun translate-stmt (stmt)
  (typecase stmt
    (nothing-stmt (make-knothing))
    (pause-stmt (make-kpause))
    (seq-stmt (let ((statements (seq-stmt-statements stmt)))
		(make-kseq :first-stmt (translate-stmt (first statements))
			   :second-stmt (translate-stmt (second statements)))))
    (par-stmt (let ((branches (par-stmt-branches stmt)))
		(make-kpar :first-branch (translate-stmt (first branches))
			   :second-branch (translate-stmt (second branches)))))
    (loop-stmt (make-kloop :body (translate-stmt (loop-stmt-body stmt))))
    (loop-each-stmt
     (translate-stmt
      (make-loop-stmt :body
		      (make-abort-stmt :signal (loop-each-stmt-signal stmt)
				       :body (make-seq-stmt :statements
							    `(,(loop-each-stmt-body stmt)
							       ,(make-halt-stmt)))))))
    (sig-decl-stmt (let ((signals (sig-decl-stmt-signals stmt)))
		    (reduce #'(lambda (sig body) (make-ksig-decl :signal sig :body body))
			    signals
			    :from-end t
			    :initial-value (translate-stmt (sig-decl-stmt-body stmt)))))
    (emit-stmt (let ((signals (emit-stmt-signals stmt)))
		 (if (null (rest signals))
		     (make-kemit :signal (first signals))
		     (translate-stmt (make-seq-stmt :statements
						    (mapcar #'(lambda (sig)
								(make-emit-stmt :signals `(,sig)))
							    signals))))))
    (present-stmt (make-kpresent :signal (present-stmt-signal stmt)
				 :then-stmt (translate-stmt (present-stmt-then-body stmt))
				 :else-stmt (translate-stmt (present-stmt-else-body stmt))))
    (suspend-stmt (make-ksuspend :signal (suspend-stmt-signal stmt)
				 :body (translate-stmt (suspend-stmt-body stmt))))
    (trap-stmt (make-ktrap :label (trap-stmt-label stmt)
			   :body (translate-stmt (trap-stmt-body stmt))))
    (exit-stmt (make-kexit :label (exit-stmt-label stmt)))
    (await-stmt
     (let ((trap-label (gensym)))
       (translate-stmt
	(make-trap-stmt :label trap-label
			:body
			(make-loop-stmt
			 :body
			 (make-seq-stmt
			  :statements
			  `(,(make-pause-stmt)
			    ,(make-present-stmt :signal (await-stmt-signal stmt)
						:then-body (make-exit-stmt :label trap-label)
						:else-body (make-nothing-stmt)))))))))
    (abort-stmt
     (let ((abort-sig (abort-stmt-signal stmt))
	   (abort-body (abort-stmt-body stmt))
	   (trap-label (gensym)))
       (translate-stmt
	(make-trap-stmt :label trap-label
			:body
			(make-par-stmt
			 :branches
			 `(,(make-seq-stmt
			     :statements
			     `(,(make-suspend-stmt :signal abort-sig
						   :body abort-body)
				,(make-exit-stmt :label trap-label)))
			   ,(make-seq-stmt
			     :statements
			     `(,(make-await-stmt :signal abort-sig)
				,(make-exit-stmt :label trap-label)))))))))
    (halt-stmt (make-kloop :body (make-kpause)))))
