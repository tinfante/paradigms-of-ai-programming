; Exercise 1.1 [m]
; Define a version of last-name that handles "Rex Morgan MD," and
; "Morton Downey, Jr.," and whatever other cases you can think of.
;
; The exercise's description uses double quotes for the inputs, so
; I'd presume the inputs are strings. But since the book has not
; described how to trim and split strings, I'll assume the inputs
; are the same as for the previous last-name function. That is,
; lists of symbols. Symbols apparently can't include commas, so I
; don't include them.
;
; the butlast function is the opposite of rest. It returns the
; input list without the last item instead of the list without
; the first item.

(defparameter *suffixes* '(MD Jr.))

(defun last-name (name)
  (if (member (first (last name)) *suffixes*)
    (last-name (butlast name))
    (first (last name))))

(last-name '(Rex Morgan MD))
(last-name '(Morton Downey Jr.))



; Exercise 1.2 [m]
; Write a function to exponentiate, or raise a number to an
; integer power. For example: (power 3 2) = 3^ = 9.

(defun repeat (x n)
  (if (= n 0)
    nil
    (cons x (repeat x (- n 1)))))

(defun power (b p)
  (if (= p 0)
    1
    (apply #'* (repeat b p))))


; A much cleaner alternative, based on the book's solution,
; is the following:

(defun power (b p)
  (if (= p 0)
    1
    (* b (power b (- p 1)))))

; The book's solution uses the cond special form (similar to
; Clojure's). Notice that the equivalent of Clojure's :defualt
; condition is: (t ( <body-for-condition> ))



; Exercise 1.3 [m]
; Write a function that counts the number of atoms in an
; expression. For example: (count-atoms ' ( a (b) c ) ) = 3.
; Notice that there is something of an ambiguity in this:
; should (a n i l c ) count as three atoms, or as two, because
; it is equivalent to (a ( ) c ) ?

(defun count-atoms (expr)
  (cond ((null expr) 0)
        ((listp expr) (+ (count-atoms (first expr)) (count-atoms (rest expr))))
        (t 1)))

; Alternatively, like the book's solution does, one can use atom
; func to check if argument is an atom instead of listp func to
; check if it's a list.



; Exercise 1.4 [m]
; Write a function that counts the number of times an expression
; occurs anywhere within another expression. Example:
;
;   (count-anywhere 'a '(a ((a) b) a)) => 3

(defun count-anywhere (sub-expr expr)
  (cond
    ((null expr) 0)
    ((listp expr) (+ (count-anywhere sub-expr (first expr))
                     (count-anywhere sub-expr (rest expr))))
    (t (if (eq sub-expr expr) 1 0))
    ))

; Gotcha! (= 'a 'a) fails with type error, comparands must be
; numbers. Use (eq 'a 'a) instead.



; Exercise 1.5 [m]
; Write a function to compute the dot product of two sequences
; of numbers, represented as lists. The dot product is computed
; by multiplying corresponding elements and then adding up the
; resulting products. Example:
;
;   (dot-product '(10 20) '(3 4 )) = 10 χ 3 + 20 χ 4 = 110

(defun dot-product (seq-1 seq-2)
  (if (or (null seq-1) (null seq-2))
    0
    (+ (* (first seq-1) (first seq-2))
       (dot-product (rest seq-1) (rest seq-2)))))


; A shorter and clearer alternative:

(defun dot-product (s1 s2)
  (apply #'+ (mapcar #'* s1 s2)))
