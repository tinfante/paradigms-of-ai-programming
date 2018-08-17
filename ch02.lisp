; Exercise 2.1 [m]
; Write a version of generate that uses cond but avoids
; calling rewrites twice.

; The following 6 functions are needed by generate func.

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for atrivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))


; Solution below. Just wrap cond form in a let form that
; binds possible-rewrites to (rewrites phrase).

(defun generate (phrase)
  (let ((possible-rewrites (rewrites phrase)))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((not (null possible-rewrites))
           (generate (random-elt possible-rewrites)))
          (t (list phrase)))))

;     ((not (null possible-rewrites)) ( <condition-body> ))
; could be written as:
;     (possible-rewrites ( <condition-body>))
; Shorter, but maybe not as clear and explicit.
;
;
; Notice, what seems to me, the extra pair of parenthesis
; around the binding list. Probably makes more sense with
; several bindings, e.g.,
;
; (let ((var1 value1)
;       (var2 value2))
;   ( <let-body> )
;   )


; The book gives the following solution,
;
; (defun generate (phrase)
;   "Generate a random sentence or phrase"
;   (let ((choices nil))
;       (cond ((listp phrase)
;              (mappend #'generate phrase))
;             ((setf choices (rewrites phrase))
;              (generate (random-elt choices)))
;             (t (list phrase)))))
;
; Notice how the choices variable is initialized to nil
; by the let form, and is then set to the value of
; (rewrites phrase) with setf. This works because setf
; creates/sets a variable, but also returns its new
; value. Another important point is that choices is a
; local variable, not a global one, even though setf is
; being used.



; Exercise 2.2 [m]
; Write a version of generate that explicitly differentiates
; between terminal symbols (those with no rewrite rules)
; and nonterminal symbols.

;   ((not (null possible-rewrites)), discussed in the
; solution to 2.1, explicitly checks if there are no
; rewrite rules. The book's solution to 2.2 is the
; following,
;
; (defun generate (phrase)
;   "Generate a random sentence or phrase"
;   (cond ((listp phrase)
;          (mappend #'generate phrase))
;         ((non-terminal-p phrase)
;          (generate (random-elt (rewrites phrase))))
;         (t (list phrase))))
;
; (defun non-terminal-p (category)
;   "True if this is a category in the grammar."
;   (not (null (rewrites category))))
;
; It's the same, only that the check is done in
; function non-terminal-p. That function's name
; ends in -p because it's a predicate. Read more
; about predicates here:
;
; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node69.html
;
; Thus, the naming convention -p for predicates
; is part of the explicitness required by exercise.



; Exercise 2.3 [h]
; Write a trivial grammar for some other language.
; This can be a natural language other than English,
; or perhaps a subset of a computer language.

(defparameter *simple-spanish-grammar*
  '((oracion -> (frase-nominal frase-verbal))
    (frase-nominal -> (frase-nominal-m _or_ frase-nominal-f))
    (frase-nominal-m -> (articulo-m nombre-m adjetivo-m+))
    (frase-nominal-f -> (articulo-f nombre-f adjetivo-f+))
    (adjetivo-m+ -> () (adjetivo-m))
    (adjetivo-m -> rojo olvidado desentrañado enterrado)
    (adjetivo-f+ -> () (adjetivo-f))
    (adjetivo-f -> sensual abandonada imaginada recuperada)
    (frase-verbal -> (adverbio-pre+ verbo adverbio-pos+ frase-nominal))
    (adverbio-pos+ -> () (adverbio-pos))
    (adverbio-pos -> silenciosamente violentamente suavemente rápidamente)
    (adverbio-pre+ -> () (adverbio-pre))
    (adverbio-pre -> nunca siempre ayer mañana)
    (articulo-m -> el un)
    (articulo-f -> la una)
    (nombre-m -> hombre ojo pensamiento huevo hambre sueño)
    (nombre-f -> pelota mujer cámara puerta mano)
    (verbo -> golpeó tomó vió disfrutó comió despegó))
  "Una gramática libre de context muy simple del español.")

(setf *grammar* *simple-spanish-grammar*)

; Below a modified version of generate function
; that can understand _or_, which I use to
; correctly match genders in nominal phrases.

(defun generate (phrase)
  (let ((possible-rewrites (rewrites phrase)))
    (cond ((listp phrase)
           (if (member '_or_ phrase)
             (generate (random-elt (remove '_or_ phrase)))
             (mappend #'generate phrase)))
          ((not (null possible-rewrites))
           (generate (random-elt possible-rewrites)))
          (t (list phrase)))))



; Exercise 2.4 [m]
; One way of describing combine-all is that it
; calculates the cross-product of the function append
; on the argument lists. Write the higher-order
; function cross-product, and define combine-all in
; terms of it.
;   The moral is to make your code as general as
; possible, because you never know what you may want
; to do with it next.

(defun repeat (x n)
  (if (zerop n)
    nil
    (cons x (repeat x (- n 1)))))

(defun cross-product (f x y)
	   (if (not y)
	       nil
	       (append (mapcar f (repeat (first y) (length x)) x)
                   (cross-product f x (rest y)))))

(defun combine-all (x y)
  (cross-product #'list x y))

; This exercise doesn't look correct. The book's
; solution uses #'append instead of #'list for
; combine-all, but that doesn't work with the
; cross-product function that it uses.

; Another alternative for the cross-product
; function, which reads like a nested loop,
; is the following,

(defun cross-product (f x y)
  "Exercise 2.4"
  (let ((r ()))
    (dolist (b y)
      (dolist (a x)
        (push (funcall f a b) r)))
    (reverse r)))
