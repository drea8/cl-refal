
;; Common Lisp REFAL Interpreter

;; This source code will also serve as a
;; basic introductory tutorial to embedding
;; Refal-like languages in Common Lisp

;; In writing our CL refal implementation
;; lets give some sample examples, lets
;; start with a simple 2 binary adder

;; this is a permutation, and cylic,
;; but for now let us treat it as a Refal
;; function.

;; to encode Refal pattern terms, we will
;; encode them in Common Lisp cells as
;; '((l_term0 r_term0)
;;   (l_term1 r_term1)
;;   ;; ...
;; )
;; ... etc

;; Here is the binary addition pattern in a Lisp SEXP

'((00 01)
  (01 10)
  (10 11)
  (11 00))

;; note this is a cyclic group

;; we want a function that takes this kind
;; of term pattern structure as parameter data,
;; an input to match it against, and the
;; Pattern Matcher Function object (unique to CL-REFAL)

;; we can call it CLREFAL-APPLY

(defun CL-REFAL-CALL (patterns input-data pattern-evaluator)
  "Apply the CL-REFAL Interpreter PATTERN-MATCHER on INPUT-DATA against PATTERNS"
  (funcall pattern-evaluator input-data patterns))  


(defun PATTERN-EVALUATOR ()
  "Pattern Logic Evaluator Control"
  `(loop for i in patterns
      if (equal (first i) input)
      return (second i))
  )

;; We can test it this way

;; We apply CL-REFAL-CALL
(cl-refal-call
;; to the cyclic binary pattern space
 '((0 1)
   (1 0))
 ;; with input member 1
 1
 ;; and the basic naive serial pattern matcher function
 (lambda (input patterns)
   ;; which goes through each pattern in the cons cells
   (loop for i in patterns
	;; checks for left term equality
      if (equal (first i) input)
	;; and returns the right term if found
      return (second i)) ) )

;; evaluate this in SLIME
;; here is the call expression without comments

(cl-refal-call
 '((0 1)
   (1 0))
 1
 (lambda (input patterns)
   (loop for i in patterns
      if (equal (first i) input)
      return (second i)) ) )

;; We would simplify this in idiomatic Common Lisp as

(funcall
 (lambda (input patterns)
   (loop for i in patterns
      if (equal (first i) input)
      return (second i)) )
 1
 '((0 1)
   (1 0)))

;; The part we are more interested in simplifying and generalizing
;; is the pattern matcher function object;

(lambda (input patterns)
  (loop for i in patterns ;; this Loop control structure/invariant,
     if (equal (first i) input) ;; this predicate match.
     return (second i)) ) ;; and this return structure are important

;; What is happening here?

;; In Common Lisp world, the expression means the following to the runtime:

;; define an anonymous function with parameters (input patterns)

;; loop through PATTERNS assuming it is a cons cell
;; , each member perform an binary check on the EQUAL function
;; , and if it matches return the SECOND of that cell

;; This loop block is the most important part, the defining an anonymous
;; function is an idiosyncracy of modular LISP.

;; We could generalize it with a LEFT-TERM and RIGHT-OUT function parameter
;; for a Pattern Matcher function object maker

(defun pattern-matcher-maker
    (left-match
     right-out)
  (eval `(lambda (input patterns)
     (loop for i in patterns
	if (funcall ,left-match input i)
	return (funcall ,right-out i)))
	))

(defun basic-left-match (x cell)
  (equal (first cell) x))

(defun basic-right-out (cell)
  (second cell))

;; We could call these this way:

(funcall
 (pattern-matcher-maker
  #'basic-left-match
  #'basic-right-out)  
 1
 '((0 1)
   (1 0)))
 
;; We could alter the basic left-right term pattern
;; matcher object this way to match for types rather than value!

(defun type-left-match (x cell)
  (typep x (first cell)))

(funcall
 (pattern-matcher-maker
  #'type-left-match
  #'basic-right-out)  
 1
 '((Symbol No)
   (Number Yes)))

;; This gives us the beginnings of a naive type system

;; At this point we should really talk about runtimes.
;; Because getting into formal modeling merits the discussion
;; of Performance optimization.

;; We have not fully implemented Turchin's original REFAL
;; with string expression and tree matching term expressions,
;; and his other included functions in the Refal standard library.

;; To cover what we want to meta-model in Refal let's go over what
;; Turchin actually included for the language.

;; Thus far we have already matched basic character strings.

(funcall
 (pattern-matcher-maker
  #'basic-left-match
  #'basic-right-out)  
 'aa
 '((a aa)
   (aa aaa)
   (aaa a)))

;; Let's include the dynamic symbol pattern matching utility Turchin had
;; in the original Refal pattern matcher object function

;; let us remember the format of the original cell pattern matching function:
;; (DEFUN BASIC-LEFT-MATCH (X CELL) (EQUAL (FIRST CELL) X))

;; First lets examine CELL

;; in the binary addition CELL would be like
;; '(0 1) or '(1 0) or '(00 01)

;; we want to process CELL values like
;; '(("A" s.a) s.a)
;; which on X of "Aardvark" would be like "ardvark"

;; We should define the full grammar of Refal expressions from
;; Turchin's own specifications, hierichally if conveniently possible.

(defun refal-left-match (x cell)
  )

;;
