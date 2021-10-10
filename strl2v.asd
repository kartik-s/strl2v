(asdf:defsystem "strl2v"
  :description "strl2v: an Esterel compiler targeting Verilog"
  :version "0.0.1"
  :author "Kartik Singh <kartik@freelygenerated.com>"
  :license "TODO"
  :components
  ((:module src
    :serial t
    :components
	    ((:file "packages")
	     (:file "ast")
	     (:file "parser")
	     (:file "kernel-ir")
	     (:file "frontend")
	     (:file "compiler")))))
