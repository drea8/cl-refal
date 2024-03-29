# CL-Refal 
A Common Lisp interpretation of Valentin Turchin's functional programming language.
Notes on the embodiment of Turchin's ideas on systems complexity, automata, and logical-linguistic semantics as expressed in Refal's flexibilities.

REFAL was a 1968 pattern-matching functional programming language invented by Russian computer scientist and cyberneticist Valentin Turchin, author of "The Phenonema of Science" and early thinker in hypercomputation.

# Install
* (load "package.lisp")
* (load "cl-refal.asd")
* (ql:quickload :cl-refal)
* or manually (load "src/interpret.lisp")

# Features
* Refal pattern matcher as an object
* compiler and automata runtime modeler

# Documentation
* see src/interpet.lisp for Refal embedding tutorial
* see test/ for examples

see "https://www.youtube.com/watch?v=DBZN7yTQ_QU" for a video from the Copenhagen research program into Turchin's supercompiler and metasystems ideas given strict automata theoretic consideration.

# To Do
* Refal expression syntax & semantick
* dynamic left var matching s. e. substitution eval
