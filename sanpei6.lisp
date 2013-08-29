;Lisp assignment 6
;Marc Sanpei, ICS313
"mutantx"

(defparameter *close-parentheses* 0)

(defparameter *nodes* '((elevator (you are in the elevator on the third floor.
                                       you can choose another floor or enter the lobby in front of you.))
                        (floor-3-lobby (you are in the floor-3-lobby.
                                            there are two rooms in front of you.
                                            simply find the sword-key pieces to kill the fortran mutant and enter a room of your choice. ))
                        (game-room (welcome to the game-room can you defeat the ada mutant and advance.
                                            this may require items from other rooms. ))
                        (trap-room (welcome to the trap-room can you avoid the traps defeat the python mutant and advance.
                                            this may require items from other rooms. ))
                        (death-room (welcome to the final room on this floor. 
                                             the death-room houses one of the most dangerous mutants.
                                             i hope you are prepared for the c++ mutant. ))))

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defparameter *edges* '((elevator (floor-3-lobby front door))
                        (floor-3-lobby (game-room front-left door)
                                       (trap-room front-right door))
                        (game-room (floor-3-lobby back door)
                                   (death-room front door)
                                   (trap-room right door))
                        (trap-room (floor-3-lobby back door)
                                   (death-room front door)
                                   (game-room left door))
                        (death-room (game-room back-left door)
                                    (trap-room back-right door))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) to the ,(cadr edge) front here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(key-sword-left key-sword-right super-armor 1-close-parentheses 2-close-parentheses 3-close-parentheses 4-close-parentheses game-system trap-tools super-shield ataricode precision-gloves super-lance super-power-up))

(defparameter *object-locations* '((key-sword-left floor-3-lobby)
                                   (key-sword-right floor-3-lobby)
                                   (super-armor floor-3-lobby)
                                   (game-system game-room)
                                   (trap-tools game-room)
                                   (super-shield game-room)
                                   (atari-code trap-room)
                                   (precision-gloves trap-room)
                                   (super-lance trap-room)
                                   (super-power-up death-room)
                                   (1-close-parentheses floor-3-lobby)
                                   (2-close-parentheses game-room)
                                   (3-close-parentheses trap-room)
                                   (4-close-parentheses death-room)))

(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'elevator)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

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

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
    (member object (cdr (inventory))))


(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory combine slay defeat disarm win))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

(defmacro new-location (name &body body)
  `(cond
   ((not (our-member ',name *nodes*))
    (pushnew '(,name (,@body)) *nodes*))
   (t "Location already exists")))

(defmacro new-path (origin destination direction path &optional (direction-back "unable"))
  `(cond
    ((or                                         
      (not (our-member ',origin *nodes*))                                      
      (not (our-member ',destination *nodes*)))
     ()"Missing location, cannot create path.")                                 
    (t(progn
        (if (equal ',direction-back "unable")
            nil    
          (cond
           ((our-member ',destination *edges*)
            (pushnew '(,origin ,direction-back ,path)
                 (cdr (assoc ',destination *edges*))))
           (t (pushnew '(,destination
                          (,origin ,direction-back ,path)) *edges*))))        
        (pushnew '(,destination ,direction ,path)
                 (cdr (assoc ',origin *edges*)))))))

(defmacro game-action (command subj obj place &body body)

  `(progn (defun ,command (subject object)	
            (if (and (eq *location* ',place)	
                     (eq subject ',subj)	
                     (eq object ',obj)	
                     (have ',subj))	
                ,@body			
            '(i cant ,command like that.)))	
          (pushnew ',command *allowed-commands*))) 


(defparameter *key-sword* nil)

(game-action combine key-sword-left key-sword-right floor-3-lobby
             (if (and (and (have 'key-sword-left) (have 'key-sword-right)) (not *key-sword*))
                 (progn (setf *key-sword* t)
                        '(the key-sword is now combined))
               '(you do not have a piece.)))

(game-action slay super-armor key-sword floor-3-lobby
             (cond ((not *key-sword*) '(you do not have combined key-sword))
                   (t '(you have slain the fortran-mutant.
                            his shell has fallen and now become a lisp programmer.
                            can you defeat the other mutants on this floor))))

(game-action defeat game-system atari-code game-room
             (cond ((not (have 'atari-code)) '(you do not have the items to win))
                   ((not (have 'game-system)) '(you do not have the items to win))
                   (t '(you have entered the atari-code and defeated the ada-mutants game.
                            he has now changed to a lisp programmer ))))

(game-action disarm trap-tools precision-gloves trap-room
             (cond ((not (have 'trap-tools)) '(you do not have the items to win))
                   ((not (have 'precision-gloves)) '(you do not have the items to win))
                   (t '(you have entered disarmed the python-mutants trap and activated it on him.
                            he has now become a lisp programmer))))

(game-action win super-power-up key-sword death-room
             (cond ((not (have 'super-power-up)) '(you do not have the items to win))
                   ((not (have 'super-shield)) '(you do not have the items to win))
                   ((not (have 'super-armor)) '(you do not have the items to win))
                   ((not (have 'super-lance)) '(you do not have the items to win))
                   (t '(congratulations you have defeated the main mutant on this floor.
                                       he has been reborn into a lisp programmer.
                                       can you clear the other floors))))
