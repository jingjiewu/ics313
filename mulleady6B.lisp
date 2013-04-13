;; mulleady6.lisp
;; floor 4

;;; Kyle Mulleady, Julie Rybarczyk, Marc Sanpei, Erick Recaido
;;; ICS 313
;;; Assignment 6
;;; Description:

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

; Edited 4/11/13. Replaced old items with new ones.
; Global variable for holding all the objects in the game.
(defparameter *objects* '(lisp-phone scripts 2-open-parentheses 2-close-parentheses))

; Edited 4/11/13. Replaced old items with new ones.
; Global variable for holding the locations of each object.
(defparameter *object-locations* '((lisp-phone hallway)
				   (scripts acting-room)
                                   (2-open-parentheses hallway)
                                   (2-close-parentheses hallway)))

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
