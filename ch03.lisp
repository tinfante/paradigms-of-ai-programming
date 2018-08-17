; Exercise 3.1 [m]
; Show a lambda expression that is equivalent to the above
; let* expression. You may need more than one lambda.

; The let* expression is:
;
; (let* ((x 6)
;        (y (* χ χ)))
;   (+ χ y))
;
; let*, in contrast to let, allows referring to a previous
; binding in the binding expression.

; Why two lambdas? The following works fine.

((lambda (x)
   (+ x (* x x)))
 6)

; With two lambdas just in case I didn't get the point of
; the exercise... (besides that let and lambdas can be
; equivalent).

((lambda (x)
   (+ x ((lambda (y)
           (* y y))
         x)))
 6)



; Exercise 3.2 [s]
; The function cons can be seen as a special case of one
; of the other functions listed previously. Which one?

; I said list, it was actually list*. Both list* and cons
; preppend the first argument to the second argument if
; the second argument is a sequence. list function, on
; the other hand, just makes a sequence where the first
; argument is the first element in the sequence, and the
; second argument the second element. Both list* and cons
; also return dotted notation when both first and second
; arguments are atoms. For example,

(cons 0 '(1 2 3))
; (0 1 2 3)

(list* 0 '(1 2 3))
; (0 1 2 3)

(list 0 '(1 2 3))
; (0 (1 2 3))

(cons 0 1)
; (0 . 1)

(list* 0 1)
; (0 . 1)

(list 0 1)
; (0 1)



; Exercise 3.3 [m]
; Write a function that will print an expression in dotted
; pair notation. Use the built-in function princ to print
; each component of the expression.

(defun print-dotted (x)
  (princ "(")
  (princ (first x))
  (princ " . ")
  (if (null (rest x))
    (princ "NIL")
    (print-dotted (rest x)))
  (princ ")"))

; Function above prints dotted notation, but returns the
; closing parenthesis. Lets try again using the books
; solution as inspiration.

(defun pd (x)
  (if (atom x)
    (princ x)
    (progn
      (princ "(")
      (pd (first x))
      (print-dot-next (rest x))
      (princ ")")
      x)))

(defun print-dot-next (x)
  (princ " . ")
  (pd x)
  )

; The key to printing the dotted notation and returing
; the list is being able to execute several expressions
; and returning the value for the last one. That's
; possible in the body for a condition with cond (as
; the book's solution uses). The if special form
; doesn't do that, one has to explicitly use progn,
; which is implicit in cond. Also note that this only
; works with (atom x) as condition, not with
; (not (listp x)).



; Exercise 3.4 [m]
; Write a function that, like the regular print function,
; will print an expression in dotted pair notation when
; necessary but will use normal list notation when
; possible.

; the print function apparently only prints the dotted
; notation for the last element of a sequence. For
; example,

(print (list 1 2 3 4 5))
; (1 2 3 4 5)

(print (list* 1 2 3 4 5))
; (1 2 3 4 . 5)

; Now lets see what happens when we use rest and there
; is only one element left in the sequence.

(print (rest (list 1 2)))
; (2)

(print (rest (list* 1 2)))
; 2

; So for list it returns a one element sequence, while
; for list* an atom, or the actual last element. The
; dot is printed inside print-dot-next function. Lets
; add a check to see if the element is a list sequence
; or an atom. We will only print the dot if x is an
; atom.

(defun print-dot-next (x)
  (cond
    ((null x) x)
    ((atom x) (princ " . ") (princ x))
    (t (princ " ") (pd (first x)) (print-dot-next (rest x)))))

; Notice that nil is also considered an atom, therefore,
; to avoid printing everything as ending in (... . NIL),
; one has to check to see if x is nil before checking if
; it's an atom.



; Exercise 3.5 [h] (Exercise in altering structure.)
; Write a program that will play the role of the guesser
; in the game Twenty Questions. The user of the program
; will have in mind any type of thing. The program will
; ask questions of the user, which must be answered yes
; or no, or "it" when the program has guessed it. If the
; program runs out of guesses, it gives up and asks the
; user what "it" was. At first the program will not play
; well, but each time it plays, it will remember the
; user's replies and use them for subsequent guesses.

; I can think of two ways of representing the data. A
; taxonimic representation:
;
; 1. ( (feline (tiger nil)
;              (lion nil)
;              (cat (bengal nil)
;                   (persian nil)
;                   (siamese nil)
;               )
;              (puma nil)
;               )
;      (canine nil) )
;
; With this data structure, if the user answer "no",
; the program would check a sibling node. So it would
; need to remember previously visited  sibling nodes.
;
; A binary tree alternative is the following:
;
; 2. (animal
;      (yes: (mammal
;              (yes: (feline
;                      (yes: (bengal nil nil)
;                       no: (canine
;                              (yes: (wolf nil nil)
;                               no: (rodent nil nil)
;                               )) ))
;               no: (bird nil nil)
;               ))
;       no: (plant
;              (yes: (tree nil nil)
;               no: (inanimate-object nil nil)
;               )) ))
;
; I added the yes: no: labels to make it easier to read.
; We could just represent the data like this:
;
;    (animal
;       (mammal
;           (feline
;               (bengal nil nil)
;               (canine (wolf nil nil)
;                       (rodent nil nil)
;                       ))
;           (bird nil nil)
;           )
;       (plant
;           (tree nil nil)
;           (inanimate-object nil nil)
;           ))
;
; This alternative has the advantage that the program
; doesn't need to remember previously visited nodes,
; and the disadvantage that it's awkward to read and
; understand.
;
; Lets try with the first approach.
;
; I tried declaring a taxonomy as a list with nested
; lists (as described above) at the global level, but
; couldn't figure out how to modify a nested node
; by reference using setf from within a function. I
; tried declaring the taxonomy variable with defvar
; and defparameter. I saw how the book solved this
; problem, and it's by using defstruct, so I'll use
; that. This means also changing the data structure.
; Now a node is defined as having a next :sibling
; and a first :child, but it's still a taxonomy
; (unlike the book's solution that uses the
; second approach).

(defstruct node
  name
  (child nil)
  (sibling nil))

(defvar taxonomy (make-node :name 'animal))
(defvar counter 0)

(defun ask (txn-node)
  (cond
    ((< counter 20)
     (setf counter (incf counter))
     (princ "is it a ") (princ (node-name txn-node)) (princ "? ")
     (let ((user-input (read)))
       (cond
         ((equal user-input 'y)
          (cond ((null (node-child txn-node))
                 (princ "What is it? ")
                 (setf (node-child txn-node) (make-node :name (read)))
                 (setf counter 0))
                (t
                 (ask (node-child txn-node)))))
         ((equal user-input 'n)
          (cond ((null (node-sibling txn-node))
                 (princ "What is it? ")
                 (setf (node-sibling txn-node) (make-node :name (read)))
                 (setf counter 0))
                (t
                 (ask (node-sibling txn-node)))))
         ((equal user-input 'it)
          (princ "In your face!!!")
          (setf counter 0)))))
    (t
     (princ "You got lucky... ")
     (setf counter 0))))

; cond was used throughout even for binary decisions
; because of its implicit progn. There is a lot of
; duplicate code that I failed to encapsulate in
; another function (couldn't modify the taxonomy
; from inside that other function). Besides the bad
; common lisp, this implementation has the following
; problems. The selection order of the next node
; is always the same. It also doesn't have the
; ability to move a node's children further down
; the taxonomy when its parent is defined under
; a more specific hypernym. One might have this
; data for example,
;
; ( (animal (canine nil)
;           (feline (tiger nil)
;                   (lion nil)
;                   (cheetah nil)
;            )
;           (cat (persian nil)
;                (siamese nil)
;            )
;            )
;   (plant nil) )
;
; If one then shows the program "cat" as a child
; of "feline", it would be nice to move the
; children of the "cat" entry already in the
; taxonomy.
;
; The use of articles could also improve.



; Exercise 3.6 [s]
; Given the following initialization for the lexical
; variable a and the special variable *b*, what will
; be the value of the let form?
;
; (setf a 'global-a)
; (defvar *b* 'global-b)
;
; (defun fn () *b*)
;
; (let ((a 'local-a)
;       (*b* 'local-b))
;   (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

; local-a, local-b, local-b, global-a, local-b
;
; Cases 3, 4 and 5 are tricky. The closure for case
; 3 happens when fn is called, so it uses local-b.
; In the 4th case I didn't know, because setf by itself
; doesn't make a special var, does it? I understood it
; only sets the value, not that it makes the var special.
; The last case is local-b because of dynamic shadowing
; of special vars.



; Exercise 3.7 [s]
; Why do you think the leftmost of two keys is the one
; that counts, rather than the rightmost?
;
; Don't know. Book's answer says that that way, when
; searching for a keyword argument, not all arguments
; have to be processed. Search can stop at the first one.



; Exercise 3.8 [m]
; Some versions of Kyoto Common Lisp (KCL) have a bug
; wherein they use the rightmost value when more than
; one keyword/value pair is specified for the same
; keyword. Change the definition of find-all so that
; it works in KCL.

; The code for find-all is,
;
; (defun find-all (item sequence &rest keyword-args
;                  &key (test #'eql) test-not &allow-other-keys)
;   "Find all those elements of sequence that match item,
;   according to the keywords. Doesn't alter sequence."
;   (if test-not
;     (apply #*remove item sequence
;            :test-not (complement test-not) keyword-args)
;     (apply #·remove item sequence
;            :test (complement test) keyword-args)))
;
; I have no idea, and no way to poke a stick at the
; problem without that version of KCL, so only
; speculating. Maybe it's possible to switch the
; evaluation order of keyword and positional
; arguments? Maybe by disallowing &allow-other-key?



; Exercise 3.9 [m]
; Write a version of length using the function reduce.

(defun rlength (l)
  (reduce (lambda (x y) (+ x 1)) l :initial-value 0))



; Exercise 3.10 [m]
; Use a reference manual or describe to figure out
; what the functions lcm and nreconc do.
;
; lcm returns the least common multiple of the
; arguments received. For example,

(lcm 25 30)
; 150

(lcm 5 3 10)
; 30

; nreconc is a reverse concatenate. Only reverses
; first list. E.g.,

(nreconc '(a b c) '(1 2 3))
; (C B A 1 2 3)



; Exercise 3.11 [m]
; There is a built-in Common Lisp function that,
; given a key, a value, and an association list,
; returns a new association list that is extended
; to include the key/value pair. What is the name
; of this function?

; It's possible to do the samething with cons,
; with the caveat that the key and value are
; one argument. E.g.,

(defvar a '((a 1) (b 2) (c 3)))
(cons '(d 4) a)
; ((D 4) (A 1) (B 2) (C 3))

; There is also acons which does exactly
; what the exercise describes.

(acons 'e 5 a)
; ((E . 5) (A 1) (B 2) (C 3))



; Exercise 3.12 [m]
; Write a single expression using format that will
; take a list of words and print them as a sentence,
; with the first word capitalized and a period after
; the last word. You will have to consult a reference
; to learn new format directives.

(format t "~@(~{~a~^ ~}~)." (list "hello" "world"))
; Hello world.
