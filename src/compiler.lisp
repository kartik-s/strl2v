;;;; top-level compiler routines

(in-package :strl2v)

(defun compile-ast (ast)
  ast)

(defun compile-prog-file (pathname)
  (with-open-file (file pathname)
    (let ((top-level-decls (loop for decl = (read file nil)
				 while decl collect decl)))
      (compile-ast (parse top-level-decls)))))
