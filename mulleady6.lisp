;;; Kyle Mulleady, Julie Rybarczyk, Marc Sanpei, Erick Recaido
;;; ICS 313
;;; Assignment 6 
;;; Description: Our World!

; Global variable from assignment one, used for the id function
(defparameter +ID+ "Kyle Mulleady, Julie Rybarczyk, Marc Sanpei, Erick Recaido")

(defun id (course-number assignment-number)
"This function prints out my name (via the +ID+ constant), the ICS
course number given as the first argument, and the assignment number
given as the second argument. Error checking is perform to make sure
both arguments are in fact nonnegative integers."
    (cond
  ((or 
	 (not (integerp course-number))		; ensures both args 
	 (not (integerp assignment-number)))	; are integers
	 '(Arguments must be integers.))
	((or
	 (< course-number 0)			; ensures both args
	 (< assignment-number 0))		; are > 0
	 '(Arguments must be nonnegative integers.))
	((format t "Name: ~a~%Course: ICS~a~%Assignment #: ~a~%" 
+ID+ course-number assignment-number))))	; prints out name, course #, assingment #

;;; Begin code for the wizard world ;;;

; Edited 4/9/13. Replaced old nodes with new ones.
; Global variable for keeping track of the nodes (locations) in the game.
; Each node has a decription that goes along with it, and will be accessed
; in key-value fashion. 
(defparameter *nodes* '((elevator (you are in the elevator- on the 4th 
			    floor. what crazy monsters could possibly 
			    be here on the top floor?))
			(hallway (you are in a hallway. you see three doors.))
			(objective-room (you are in the objective-room. maybe 
			    you should c what your objective is. oh no! a 
			    monster! what is he doing? is he...is he pretending 
			    to make smalltalk with you? why is he being so verbose? 
			    oh- its the objective-c monster! maybe you can scare him 
                            by showing him the latest and greatest mobile device.))
			(acting-room (you are in the acting room. you see a monster 
			    on stage. he seems to have the audience thinking he's 
			    acting impromptu- but you know better--hes really just 
			    reading a script! its the php monster! you know what 
			    you have to do.))
			(theater (you are in the theater. you see a giant monster 
			    watching talladega nights. it appears that- like Ricky 
			    Bobby- he just wants to go fast...its the c mutant! 
			    the largest mutant of all! are you ready to convert him?))))

(defun describe-location (location nodes)
"This function describes the user's current location. It is passed
the current location as the first parameter, and the global variable
*nodes* as the second parameter. It calls assoc with location as the
key in order to get the associated value/description."
   (cadr (assoc location nodes)))

; Edited 4/9/13. Replaced old edges with new ones.
; Global variable that associates each node with edges to adjacent nodes.
(defparameter *edges* '((elevator (hallway south door))
			(hallway (elevator north elevator-door)
				(objective-room west door)
				(acting-room east door)
				(theater south door))
			(acting-room (hallway west door))
			(theater (hallway north door))
			(objective-room (hallway east door))))

(defun describe-path (edge)
"This function gets called only as a helper function by the similar
describe-paths. It takes care of the formatting, while describe-paths
takes care of calling each edge for each location."
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
"This is a lot of function wrapped in a single line of code. apply is
used to send the append function as an argument to the rest of the code.
append is used so there is only one set of parentheses in the output. 
mapcar is used to send describe-path to the rest of the code. describe-
path is used to make a human-readable sentence out of the edges adjacent
to the user's current location."
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; Edited 4/11/13. Replaced old items with new ones.
; Global variable for holding all the objects in the game.
(defparameter *objects* '(lisp-phone scripts 2-open-parentheses 2-close-parentheses))

; Edited 4/11/13. Replaced old items with new ones.
; Global variable for holding the locations of each object.
(defparameter *object-locations* '((lisp-phone hallway)
				   (scripts acting-room)
                                   (2-open-parentheses hallway)
                                   (2-close-parentheses hallway)))


(defun objects-at (loc objs obj-loc)
"This function looks at each object and its respective
location. If the location of the current object being checked
does not match the user's location, that object gets removed
from the list. This is a helper function to describe-objects."
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
"This function tells the user which objects are in the current
location. mapcar #'describe-obj uses the returned object from the 
call to objects-at to display what the user sees on the floor."
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

; Global variable that sets up the starting location and
; keeps track of the user's location throughout the game.
(defparameter *location* 'elevator)

(defun look ()
"This function tells the user about everything around him. It
uses append to reduce the amount of parentheses in the output, and
calls the three functions to describe the wizard world to the user."
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))


(defun walk (direction)
"This function allows the user to walk through the world. It first checks
to make sure the input matches the cadr of, for example, (garden west door).
If not, it tells the user he cannot go that way. Otherwise it takes the car
of, for example, (garden west door) and makes that the new *location*, then
calls look."
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

; Edited 4/11/13. Original code to pick up an object is still here. Added
; conditions for when user picks up open-parentheses and close-parentheses.
(defun pickup (object)
"This function picks up the object the user requests. It uses objects-at to 
make sure it was a valid request. If so, it pushes the object onto the list
with 'body. If not, it tells the user of the invalid request."
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (cond ((equal (subseq (string object) 2) (string 'open-parentheses))
                (setq *open-parentheses* (+ (parse-integer (subseq (string object) 0 1)) *open-parentheses*))
                `(you now have ,*open-parentheses* open-parentheses.))
               ((equal (subseq (string object) 2) (string 'close-parentheses))
                (setq *close-parentheses* (+ (parse-integer (subseq (string object) 0 1)) *close-parentheses*))
                `(you now have ,*close-parentheses* close-parentheses.))
               ((push (list object 'body) *object-locations*)
               `(you are now carrying the ,object))))
               (t '(you cannot get that.))))

; Edited 4/11/13. Parentheses are not part of the 'body. They needed
; to be cons'ed in.
(defun inventory ()
"This function simply tells the user what he is carrying. It does so by
using 'body as the location in the call to objects-at, and using a cons
with items as the car and the returned list from objects-at as the cdr."
(cons
 (cons 
  (cons 
   'items- (objects-at 'body *objects* *object-locations*))
  `(,*open-parentheses* open-parentheses))
 `(,*close-parentheses* close-parentheses)))

(defun have (object) 
"This function tells the user whether or not he is carrying the
object specified. It calls the cdr of inventory to do so. The car of
inventory is 'items-'. The cdr is the list of objects the user is carrying."
    (member object (cdr (flatten (inventory)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Kyle Mulleady
;;; ICS 313
;;; Assignment 4
;;; Description: Wizard Game Part 2. This file contains
;;;  the addition of the second part of the wizard's world,
;;;  along with pieces for the multi-part object and the new 
;;;  friendly interface. 

;;; Also, this is where Lisp get crazy. Some of these functions
;;; are so hard to follow, especially tweak-text.

(defun game-repl ()
"This function sets up our new REPL. It simply checks
to make sure the user did not enter 'quit', and then
evaluates, prints, and loops via a recursive call to this function."
    (let ((cmd (game-read)))             ; Let statement for cmd variable getting value of game-read.
        (unless (eq (car cmd) 'quit)     ; True if user doesn't enter 'quit'.
            (game-print (game-eval cmd)) ; Evaluate user's command and print result.
            (game-repl))))               ; Recursion.

(defun game-read ()
"This function reads the last user-entered string and adds an open-parenthesis to
the beginning and a close-parenthesis to the end. It then uses an flet statement
to create a function that adds a quote to the beginning of all parameters of the 
user's command."
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")")))) ; Add parens.
         (flet ((quote-it (x)                                  ; Flet statememt.
                    (list 'quote x)))                          ; Turn x into (quote x).                  
             (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))) ; Cons statement to have the car of
                                            ; cmd appended by a quoted version of every element in cdr.

; Global variable for commands the user can enter.
(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
"This is a simple function that evaluates the parameter as a command
if it is in *allowed-commands*. Otherwise the function tells the user
that it does not know that command."
    (if (member (car sexp) *allowed-commands*) ; Check if parameter is in *allowed-commands*.
        (eval sexp)                            ; If so, evaluate it.
        '(i do not know that command.)))       ; If not, tell the user it can't.

(defun tweak-text (lst caps lit)
"This function is crazy. Its purpose is to capitalize the first letter of
every sentence. It does this via a very complex cond statement and conses."
  (when lst                                                            ; If lst is not null, continue execution.
    (let ((item (car lst))                                             ; Let item be the car of lst.
          (rest (cdr lst)))                                            ; Let rest be the cdr of lst.
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit))) ; If item is a space, cons it with recursive call with rest caps lit.
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) ; If item is ! or ? or . cons it with recursive call with rest t lit. 
            ((eql item #\") (tweak-text rest caps (not lit)))          ; If item is a double quote, make recursive call with rest caps (not lit).
            (lit (cons item (tweak-text rest nil lit)))                ; If lit is not null, cons item with recursive call with rest nil lit.
            (caps (cons (char-upcase item) (tweak-text rest nil lit))) ; If caps is not null, cons (char-upcase item) with recursive call with rest nil lit.
            (t (cons (char-downcase item) (tweak-text rest nil nil))))))) ; If all other conditions are not met, cons (char-downcase item) with
                                                                       ; recursive call with rest nil nil.

; Edited 4/11/13. Added the call to the remove function to remove close-parentheses
; that started appearing with the edit of the inventory function.
(defun game-print (lst)
"This is another tricky function. Its purpose is to take its input and format it into proper 
English grammar (i.e. begin with a capital letter and get rid of quotes and parentheses) for 
the user. It does this by first trimming the parentheses off the ends. It then coerces each 
character into a list, passes the list to tweak-text along with t and nil, and coerces the 
result back to a string."
    (princ (remove #\) (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))) ; This is the line the doc-string explains.
    (fresh-line)) ; Newline if necessary.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; Kyle Mulleady, Julie Rybarczyk, Marc Sanpei
;;; ICS 313
;;; Assignment 5
;;; Description: Macros! This file adds more features
;;;  to the wizard's world from the previous assignments.
;;;  Some macros were given, and we added our custom macro.

(defmacro game-action (command subj obj place &body body)
"This macro creates a function! The command argument becomes the
name of the function. Error checks are performed against remaining 
arguments. If all error checks pass, the body is executed and
command is added to *allowed-commands*." 
  `(progn (defun ,command (subject object)	; Create a function header.
            (if (and (eq *location* ',place)	; Error check for location.
                     (eq subject ',subj)	; Error check for subject.
                     (eq object ',obj)		; Error check for object.
                     (have ',subj))		; Error check inventory.
                ,@body				; Body of macro.
            '(i cant ,command like that.)))	; Execute if errors exist.
          (pushnew ',command *allowed-commands*))) ; Otherwise, add command.

(defmacro new-object (object location)
"This macro adds a new object to the world. It takes an
object and a location as a parameter. Error checks are made to
ensure the object is a symbol and doesn't already exist, and 
that the location does exist."
  `(cond
    ((not (symbolp ,object))		; If object is not a symbol,
     '(object must be a symbol.))	; inform user.
    ((member ,object *objects*)		; If object already exists,
     '(object already exists.))		; inform user.
    ((not (assoc ,location *nodes*))	; If location doesn't exist,
     '(invalid location.))		; inform user.
    ((push ,object *objects*)		; Otherwise, add object and 
     (push (list ,object ,location) *object-locations*) ; object location.
     '(new object added.))))		; Give user confirmation.

(defmacro new-location (location description)
"This macro adds a new location to the world. Location must
be a symbol and description must be a list."
  `(cond
    ((not (symbolp ,location))		; If location is no a symbol,
     '(location must be a symbol.))	; inform user.
    ((assoc ,location *nodes*)		; If location already exists,
     '(location already exists.))	; inform user.
    ((null ,description)		; If description is empty list,
     '(description cannot be null.))	; inform user.
    ((not (listp ,description))		; If description is not a list,
     '(new location needs a description in list form.)) ; inform user.
    ((push (list ,location ,description) *nodes*) ; Otherwise, add 
     '(new location added.)))) ; location and description to *nodes*.

(defun edges (edge)
"This is a helper function for edge-exists. It 
extracts edge from the list of (node edge path)."
  (cadr edge))

(defun edge-exists (location direction)
"This function performs an error check for new-path. It checks if
the user is trying to add to a node an edge that already exists."
  (member direction (mapcar #'edges (cdr (assoc location *edges*)))))
   
(defmacro new-path (location-from location-to direction path)
"This macro adds a new path to the world. Location-from and 
location-to must be existing locations. Direction cannot be an
existing edge at that location. Direction and path must be a symbol."
    `(cond
      ((not 
        (or
          (assoc ,location-from *nodes*)	; If either location
          (assoc ,location-to *nodes*)))	; doesn't exist,
       '(you must enter existing locations.))	; inform user.
      ((or (not (symbolp ,direction))		; If direction or path
           (not (symbolp ,path)))		; is not a symbol,
       '(direction and path must be symbols.))	; inform user.
      ((edge-exists ,location-from ,direction)	; If the edge already exists
       '(that path is taken.))			; at this node, inform the user.
      ((nconc (assoc ,location-from *edges*) (list (list ,location-to ,direction ,path)))
       '(new path added. add opposite path if you want a 2-way edge.)))) ; Otherwise, add edge to *edges*.

; New items to add to *allowed-commands*.
(defparameter *new-allowed-commands* '(new-object new-location new-path help h ?))
(nconc *allowed-commands* *new-allowed-commands*)

(defun help () 
"This function prints out a help menu."
  (game-print '(---))
  (game-print '(you may type quit or one of the following commands--)) 
  (game-print *allowed-commands*)
  (game-print '(---))
  (game-print '(Follow this syntax for our custom macros--))
  (game-print '(new-object my-new-object my-location))
  (game-print '(new-location my-new-location "("description of my new location")"))
  (game-print '(new-path from to direction path))
  '---)
(defun h ()
"Shortcut to help function."
  (help))
(defun ? ()
"Shortcut to help function."
  (help))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; Kyle Mulleady, Julie Rybarczyk, Marc Sanpei, Erick Recaido
;;; ICS 313
;;; Assignment 6
;;; Description:

(defparameter *open-parentheses* 0)
(defparameter *close-parentheses* 0)

(defparameter *current-floor* 1)

(defparameter *newest-allowed-commands* '(show unscriptify convert))
(nconc *allowed-commands* *newest-allowed-commands*)

(defun show (monster item)
  (cond
   ((and 
     (eq *location* 'objective-room)
     (have 'lisp-phone)
     (eq monster 'objective-c-monster) 
     (eq item 'lisp-phone))
    '(the objective-c monster is terrified! he surrenders to the beauty
of the new phone and becomes a proud lisper.))
   (t '(thats not how its done. try and defeat him another way...))))

(defun flatten (l)
"This function flattens nested lists. It gets called by the 'have' function
at the top of this file. It's necessary since we changed the inventory function."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun unscriptify (monster item)
  (cond
   ((and
     (eq *location* 'acting-room)
     (eq monster 'php-monster)
     (have 'scripts)
     (eq item 'scripts))
    '(the php monster is ashamed of his scripting ways. he renounces scripting
and adopts functional programming and lisp yaayyyyy!))
   (t '(you are not prepared to convert the php monster. he will continue his scripting
ways until you have what it takes to show him the bright side of programming that is lisp!))))

(defun convert (monster)
  (cond
   ((and
     (eq *location* 'theater)
     (eq monster 'c-mutant)
     (eq 10 *open-parentheses*)
     (eq 10 *close-parentheses*))
    '(the c mutant blah blah you win!))
   (t '(the c mutant looks at you and laughs. you dont even have matching amounts of parentheses- he says. you lose.)))) 

