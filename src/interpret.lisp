
;; Common Lisp REFAL Interpreter

;; Valentin Turchin was a Russian cyberneticist who
;; wrote a programming language called "REFAL",
;; short for Recursive Evaluation of Functions Language,
;; for his research in physics, automata, natural language translation,
;; artificial intelligence, and complexity theory.

;; Because of its elegance and simplicity, I
;; consider it one of the most 'pure' functinal programming
;; language models. This simplicity and many of
;; Turchin's other design choices make it possibly
;; easier to extend via metaprogramming and compiler
;; construction than Lisp, Haskell, or Prolog.
;; This repository will test this idea.

;; There is a robust implementation online maintained
;; by a Russian technical university but we will
;; analyze the ideas behind Refal by implementing
;; a subset of it in Common Lisp

;; This file interpret.lisp will serve as a
;; introductory tutorial to embedding
;; Refal-like languages in Common Lisp, author uses
;; Steel Bank Common Lisp, Emacs, and SLIME
;; and assumes knowledge of basic Common Lisp
;; I may include notes that seem trivial to the
;; advanced programmer, excuse them they are for
;; my own thoughts or beginner's study.


;; Introduction to Refal

;; A Refal program is a recursive List (or set thereof)
;; of Sentences, formed by Left and Right terms
;; that forms a Function mapping from Predicates
;; satisfies on the Left to results returned by their
;; Right term in the same Sentence.

;; This is what the original Refal looks like

"BinAdd {
 '0' '0' = '0';
 '0' '1' = '1';
 '1' '0' = <BinAdd '0' '1'>;
 '1' '1' = '10';
}"


;; The default operation in Refal is concatenation, so
;; '0' '0' is equivalent to the Lisp list (or string) '(0 0) or "00"
;; This is what it will look like embedded in Common Lisp Refal

'(def binadd
  ((0 0) 00)
  ((0 1) 01)
  ((1 0) (binadd 0 1))
  ((1 1) 10))

;; and would be called like

'(binadd 0 0)

;; from the Refal source material by Valentin Turchin

"Refal Function Execution	  

We need to clarify the process of Refal function execution now.

1. A sentence is elected from the left side Patterns for which you can get a function argument by changing the variables in it to some values. If no such Sentence exists, then the program ends with an error of recognition impossible

2. The variables values are fixed when they request to the function argument when they are placed in the left part of the selected sentence, if there are several such sets of variables values (permutations), then its fixed the one in which the leftmost e-variable takes the shortest value. If it does not resolve ambiguities, then the next e-variable is considered and so on.

3. The variables are replaced by their values in the right side of the selected sentence. Then the functions on the right are calculated."


;; to encode Refal Sentence pattern terms, we will
;; write them in Common Lisp cells as

'(
  (l_term0 r_term0)
   (l_term1 r_term1)
   ;; ...etc
  )

;; In writing our CL refal implementation
;; lets give some sample examples, lets
;; start with a simple binary increment

;; Here is the binary increment pattern in a Lisp SEXP

'((00 01)
  (01 10)
  (10 11)
  (11 00))

;; Note this is a cylic group,
;; but for now let us focus on it as a Refal function pattern

;; This pattern with an input of 00 returns 01 etc

;; We want a function that takes this kind
;; of Sentence terms structure as parameter data,
;; an input to match it against, and the
;; Pattern Matcher Function object (unique to CL-REFAL)

(defun cl-refal-call (patterns input-data pattern-evaluator)
  (funcall pattern-evaluator input-data patterns))  


(defun pattern-evaluator ()
  `(loop for i in patterns
      if (equal (first i) input)
      return (second i)))  

;; We can test it this way

;; We apply CL-REFAL-CALL
(cl-refal-call
;; to the cyclic binary space
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

;; Refal terms requiring lists of string checks can
;; be modeled as language acceptors (some set of passing strings)
;; and thus a compiler for Refal understands the string
;; acceptors of term predicates and optimizes for traversal
;; of the ideal runtime for some performance feature.

;; So eventually we want an Automata theoretic model of
;; valid Refal programs and their compilation "morphogenesis",
;; how more complex Refal compilers grow in runtime complexity
;; for optimizing performance features of choice.

;; The runtime complexity grows with the logical-semantic
;; complexity and length of terms in the pattern matching
;; structure of the Refal function patterns and the
;; runtime structure of the Refal pattern matcher object,
;; which merits study in its own right.

;; Before doing so we need to implement the full REFAL syntax,
;; as we have not fully implemented Turchin's original REFAL
;; with string expression and tree matching term expressions,
;; and his other included functions in the Refal standard library.

;; To cover what we want to meta-model in Refal let's go over what
;; Turchin actually included for the language.

;; Thus far we have already matched basic character lists.

(funcall
 (pattern-matcher-maker
  #'basic-left-match
  #'basic-right-out)  
 'a
 '((a ab)
   (ab aba)
   (aba acba)))

;; Let's include the dynamic symbol pattern matching utility Turchin had
;; in the original Refal pattern matcher object function

;; CELL values like

'(("A" s.a) s.a))

;; which on X of "Aardvark" would be like "ardvark"

;; expanding the above expr '(("A" s.a) s.a)
;; is the automated equivalent of what would be produced
;; if we could manually write every

'(("A" s.[0]) s.[0])
  ("A" s.[1]) s.[1]) ;; ... etc
)

;; term that could be expanded by
;; the s. set

;; s.a delineates a Variable Determinate, The a variable
;; s.b would be a similar dynamic matcher disjunct and on the b variable

'((s,a " " s.b) (s.a s.b))

;; on "a b" return '(a b)
;; on "1 2" return '(1 2)

;; let us remember the format of the original cell pattern matching function:
;; (DEFUN BASIC-LEFT-MATCH (X CELL) (EQUAL (FIRST CELL) X))

;; First lets examine CELL

;; in the binary addition CELL would be like
;; '(0 1) or '(1 0) or '(00 01)


;; We should define the full grammar of Refal expressions from
;; Turchin's own specifications, hierichally if conveniently possible.

;; see /doc/ for Refal function reference

;; Here is the Refal grammar;

;; the default operation is list concatenation!

;; lets start with flat strings,
;; then later work on Structure Brackets
;; (non flat structures), later graphs and
;; paradiscrete structures

'(s.1 s.2 s.3)
;; term forms any three symbols like
;; 'ABC'

;; term pair
'((s.1 s.2 s.3) (s.3 s.2))
;; returns 'CB' from INPUT 'ABC'

'(s.A s.A s.A)
;; term repairs any three identical symbols like '666' or 'www'

'((s.A s.A s.A) s.A)
;; from 'www' returns 'w'


'(s.Edge s.Middle s.Edge)
;;- first and last must match, ie 'kek' or '^_^'

;; Note the natural language semantic name binds we have
;; for these variables "edge" "middle" and so forth.

;; Keep these natural language tokens in mind
;; for when we fully expand the semantics of a Refal
;; pattern match reader object.

;; In our analysis of Refal programs, at some point we
;; want to have a model of the "programmer's reader",
;; capable of comprehension of symbolic tokens like
;; "s.Edge" and "s.Middle" in the source outright
;; from first glance following basic Latin alphabet
;; and English comprehension training.

'((s.Edge e s.Edge) s.Edge)
;; with INPUT '404' returns '4'

					
'(s.first e.middle s.last)
;; accepts any expression containing at least two symbols
;; e.middle could be a further string, in this case procedurally the first and last characters of the list input buffer are first checked (as a type), a Refal Pattern Type

;; e. expressions can also null, for above the middle e.middle can be nulle so ++ or -+ is valid for s.first and s.last values
;; non-abbreviated symbols and strs are literal like

'(s.first 'bc') ;; matches any string starting with s.first and terminating with 2 chars bc

'(e.Eq e.Eq)
;; is an expression with even length, which can be divided into two identifical halves 'ABCABC' or '8888' or the empty expression (divides into two empty ones)

;; So with our Refal expression syntax space defined, let's translate
;; the definition into a Common Lisp left match predicate function

(defun refal-left-match (x cell)
  )



;; Advanced CL Refal Extensions
;;

;; Infinite Refal Sentences

;; Non-Flat Structure Matching

;; Refal Pattern Matching morphogenesis
