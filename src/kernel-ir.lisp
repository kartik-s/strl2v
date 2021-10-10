;;;; Esterel kernel language IR

(in-package :strl2v)

(defstruct (kernel-node (:copier nil)))

;; top-level kernel program node
(defstruct (kprogram (:include kernel-node) (:copier nil))
  modules)

;; kernel expressions
(defstruct (ksig (:include kernel-node) (:copier nil))
  name)

;; kernel statements
(defstruct (kmodule (:include kernel-node) (:copier nil))
  name
  inputs
  outputs
  body)
(defstruct (knothing (:include kernel-node) (:copier nil)))
(defstruct (kpause (:include kernel-node) (:copier nil)))
(defstruct (kseq (:include kernel-node) (:copier nil))
  first-stmt
  second-stmt)
(defstruct (kpar (:include kernel-node) (:copier nil))
  first-branch
  second-branch)
(defstruct (kloop (:include kernel-node) (:copier nil))
  body)
(defstruct (ksig-decl (:include kernel-node) (:copier nil))
  signal
  body)
(defstruct (kemit (:include kernel-node) (:copier nil))
  signal)
(defstruct (kpresent (:include kernel-node) (:copier nil))
  signal
  then-stmt
  else-stmt)
(defstruct (ksuspend (:include kernel-node) (:copier nil))
  signal
  body)
(defstruct (ktrap (:include kernel-node) (:copier nil))
  label
  body)
(defstruct (kexit (:include kernel-node) (:copier nil))
  label)
