(defvar *ops* nil "A list of available operators.")

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
(member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op) 
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x) 
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member-equal goal (op-add-list op)))


(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))



(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
         :preconds '(son-at-home car-works)
         :add-list '(son-at-school)
         :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
         :preconds '(car-needs-battery shop-knows-problem shop-has-money)
         :add-list '(car-works))
    (make-op :action 'tell-shop-problem
         :preconds '(in-communication-with-shop)
         :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
         :preconds '(know-phone-number)
         :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
         :preconds '(have-phone-book)
         :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
         :preconds '(have-money)
         :add-list '(shop-has-money)
         :del-list '(have-money))
    (make-op :action 'work
         :preconds '(have-time)
         :add-list '(have-money)
         :del-list '(have-time))
    ))

(load "helper")
(mapc #'convert-op *school-ops* )
(setf *ops* *school-ops*)
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))
; ((START)
;  (EXECUTING LOOK-UP-NUMBER)
;  (EXECUTING TELEPHONE-SHOP)
;  (EXECUTING TELL-SHOP-PROBLEM)
;  (EXECUTING GIVE-SHOP-MONEY)
;  (EXECUTING SHOP-INSTALLS-BATTERY)
;  (EXECUTING DRIVE-SON-TO-SCHOOL))

(debug :gps)

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))
; Goal: SON-AT-SCHOOL
; Consider: DRIVE-SON-TO-SCHOOL
;   Goal: SON-AT-HOME
;   Goal: CAR-WORKS
;   Consider: SHOP-INSTALLS-BATTERY
;     Goal: CAR-NEEDS-BATTERY
;     Goal: SHOP-KNOWS-PROBLEM
;     Consider: TELL-SHOP-PROBLEM
;       Goal: IN-COMMUNICATION-WITH-SHOP
;       Consider: TELEPHONE-SHOP
;         Goal: KNOW-PHONE-NUMBER
;         Consider: LOOK-UP-NUMBER
;           Goal: HAVE-PHONE-BOOK
;         Action: LOOK-UP-NUMBER
;       Action: TELEPHONE-SHOP
;     Action: TELL-SHOP-PROBLEM
;     Goal: SHOP-HAS-MONEY
;     Consider: GIVE-SHOP-MONEY
;       Goal: HAVE-MONEY
;     Action: GIVE-SHOP-MONEY
;   Action: SHOP-INSTALLS-BATTERY
; Action: DRIVE-SON-TO-SCHOOL
; ((START)
;  (EXECUTING LOOK-UP-NUMBER)
;  (EXECUTING TELEPHONE-SHOP)
;  (EXECUTING TELL-SHOP-PROBLEM)
;  (EXECUTING GIVE-SHOP-MONEY)
;  (EXECUTING SHOP-INSTALLS-BATTERY)
;  (EXECUTING DRIVE-SON-TO-SCHOOL))

(undebug)

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school have-money))
; NIL

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school))
; NIL

(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school))
; NIL

(gps '(son-at-home) '(son-at-home))
; ((START))

(gps '(son-at-home car-needs-battery have-money have-phone-book have-time)
     '(have-money son-at-school))
; NIL

; Notice the problem in the  next two calls to gps. If have-money
; comes first in the list of goals, it is considered achieved but
; then it isn't reconsidered after the son-at-school goal uses
; have-money as pre-condition for fixing the car.

(gps '(son-at-home car-needs-battery have-money have-phone-book have-time)
     '(have-money son-at-school))
; NIL


(gps '(son-at-home car-needs-battery have-money have-phone-book have-time)
     '(son-at-school have-money))
; ((START) 
;  (EXECUTING LOOK-UP-NUMBER)
;  (EXECUTING TELEPHONE-SHOP)
;  (EXECUTING TELL-SHOP-PROBLEM)
;  (EXECUTING GIVE-SHOP-MONEY)
;  (EXECUTING SHOP-INSTALLS-BATTERY)
;  (EXECUTING DRIVE-SON-TO-SCHOOL)
;  (EXECUTING WORK))



; The New Domain Problem: Monkey and Bananas

(defparameter *banana-ops*
  (list
    (op 'climb-on-chair
        :preconds '(chair-at-middle-room at-middle-room on-floor)
        :add-list '(at-bananas on-chair)
        :del-list '(at-middle-room on-floor))
    (op 'push-chair-from-door-to-middle-room
        :preconds '(chair-at-door at-door)
        :add-list '(chair-at-middle-room at-middle-room)
        :del-list '(chair-at-door at-door))
    (op 'walk-from-door-to-middle-room
        :preconds '(at-door on-floor)
        :add-list '(at-middle-room)
        :del-list '(at-door))
    (op 'grasp-bananas
        :preconds '(at-bananas empty-handed)
        :add-list '(has-bananas)
        :del-list '(empty-handed))
    (op 'drop-ball
        :preconds '(has-ball)
        :add-list '(empty-handed)
        :del-list '(has-ball))
    (op 'eat-bananas
        :preconds '(has-bananas)
        :add-list '(empty-handed not-hungry)
        :del-list '(has-bananas hungry))
    (op 'walk-from--middle-room-to-door
        :preconds '(at-middle-room on-floor)
        :add-list '(at-door)
        :del-list '(at-middle-room))
  ))


(setf *ops* *banana-ops*)


(GPS '(at-door on-floor has-ball hungry chair-at-door) '(not-hungry))
; ((START)
;  (EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
;  (EXECUTING CLIMB-ON-CHAIR) (EXECUTING DROP-BALL)
;  (EXECUTING GRASP-BANANAS) (EXECUTING EAT-BANANAS))

(GPS '(at-middle-room on-floor has-ball hungry chair-at-door) '(not-hungry))
; ((START)
;  (EXECUTING WALK-FROM--MIDDLE-ROOM-TO-DOOR)
;  (EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
;  (EXECUTING CLIMB-ON-CHAIR) (EXECUTING DROP-BALL)
;  (EXECUTING GRASP-BANANAS) (EXECUTING EAT-BANANAS))



; The Maze Searching Domain

(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
     '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
       (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
       (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))
     ))

(defun find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results
                                  :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))"
  (fifth (second action)))


(setf *ops* *maze-ops*)

(find-path 1 25)
; (1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25)

(find-path 1 1)
; (1)

(equal (find-path 1 25) (reverse (find-path 25 1)))
; T



; The Blocks World Domain

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))


(setf *ops* (make-block-ops '(a b)))

(gps '((a on table) (b on table) (space on a) (space on b) (space on table))
     '((a on b) (b on table)))
; ((START)
;  (EXECUTING (MOVE A FROM TABLE TO B)))


(debug :gps)

(gps '((a on b) (b on table) (space on a) (space on table))
     '((b on a)))
; Goal: (B ON A)
; Consider: (MOVE B FROM TABLE TO A)
;   Goal: (SPACE ON B)
;   Consider: (MOVE A FROM B TO TABLE)
;     Goal: (SPACE ON A)
;     Goal: (SPACE ON TABLE)
;     Goal: (A ON B)
;   Action: (MOVE A FROM B TO TABLE)
;   Goal: (SPACE ON A)
;   Goal: (B ON TABLE)
; Action: (MOVE B FROM TABLE TO A)
; ((START)
;  (EXECUTING (MOVE A FROM B TO TABLE))
;  (EXECUTING (MOVE B FROM TABLE TO A)))


(undebug)

(setf *ops* (make-block-ops '(a b c)))

(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((b on a) (c on b)))
; ((START)
;  (EXECUTING (MOVE A FROM B TO TABLE))
;  (EXECUTING (MOVE B FROM C TO A))
;  (EXECUTING (MOVE C FROM TABLE TO B)))

(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((c on b) (b on a)))
; NIL


(defun achieve-all (state goals goal-stack)
  "Achieve each goal, trying several orderings."
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun orderings (l) 
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(gps '((c on a) (a on table) (b on table) (space on c) (space on b)
                (space on table))
     '((c on table)))
; ((START)
;  (EXECUTING (MOVE C FROM A TO B))
;  (EXECUTING (MOVE C FROM B TO TABLE)))

(gps '((c on a) (a on table) (b on table) (space on c) (space on b)
                (space on table))
     '((c on table) (a on b)))
; ((START)
;  (EXECUTING (MOVE C FROM A TO B))
;  (EXECUTING (MOVE C FROM B TO TABLE))
;  (EXECUTING (MOVE A FROM TABLE TO C))
;  (EXECUTING (MOVE A FROM C TO B)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state))))) ;***


(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, 
  sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op) 
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))

(gps '((c on a) (a on table) (b on table) (space on c) (space on b)
                (space on table))
     '((c on table) (a on b)))
; ((START)
;  (EXECUTING (MOVE C FROM A TO TABLE))
;  (EXECUTING (MOVE A FROM TABLE TO B)))

(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((b on a) (c on b)))
; ((START)
;  (EXECUTING (MOVE A FROM B TO TABLE))
;  (EXECUTING (MOVE B FROM C TO A))
;  (EXECUTING (MOVE C FROM TABLE TO B)))

(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((c on b) (b on a)))
; ((START)
;  (EXECUTING (MOVE A FROM B TO TABLE))
;  (EXECUTING (MOVE B FROM C TO A))
;  (EXECUTING (MOVE C FROM TABLE TO B)))


(setf start '((c on a) (a on table) (b on table) (space on c) (space on b)
              (space on table)))

(gps start '((a on b) (b on c)))
; NIL

(gps start '((b on c) (a on b)))
; NIL
