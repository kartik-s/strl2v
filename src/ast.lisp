;;;; AST node definitions

(in-package :strl2v)

(defstruct ast-node
  source-file
  source-pos)

;; top-level program node
(defstruct (program (:include ast-node) (:copier nil))
  modules)

;; expressions
(defstruct (signal-name (:include ast-node) (:copier nil))
  name)

;; statements
(defstruct (module-def (:include ast-node) (:copier nil))
  name
  inputs
  outputs
  body)
(defstruct (nothing-stmt (:include ast-node) (:copier nil)))
(defstruct (pause-stmt (:include ast-node) (:copier nil)))
(defstruct (seq-stmt (:include ast-node) (:copier nil))
  statements)
(defstruct (par-stmt (:include ast-node) (:copier nil))
  branches)
(defstruct (loop-stmt (:include ast-node) (:copier nil))
  body)
(defstruct (loop-each-stmt (:include ast-node) (:copier nil))
  signal
  body)
(defstruct (sig-decl-stmt (:include ast-node) (:copier nil))
  signals
  body)
(defstruct (emit-stmt (:include ast-node) (:copier nil))
  signals)
(defstruct (present-stmt (:include ast-node) (:copier nil))
  signal
  then-body
  else-body)
(defstruct (suspend-stmt (:include ast-node) (:copier nil))
  signal
  body)
(defstruct (trap-stmt (:include ast-node) (:copier nil))
  label
  body)
(defstruct (exit-stmt (:include ast-node) (:copier nil))
  label)
(defstruct (await-stmt (:include ast-node) (:copier nil))
  signal)
(defstruct (abort-stmt (:include ast-node) (:copier nil))
  signal
  body)
(defstruct (halt-stmt (:include ast-node) (:copier nil)))
