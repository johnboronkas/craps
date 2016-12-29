;; Seed the random number generator.
(setf *random-state* 
  (make-random-state t))

(defun rollDie ()
  "Rolls one dice, returning a value between 1 and 6."
  (+ (random 6) 1))

(defun rollDice ()
  "Rolls two dice, returning both of their values."
  (list
    (rollDie)
    (rollDie)))

(defun sum (numberList)
  "Returns the sum of a list."
  (reduce #'+ numberList))

(defun playRound ()
  "Plays a round of craps.
  Returns t if the round was won, NIL if the round of lost
  and returns a list of the dice rolls."
  (let ((diceValue
	  (sum (rollDice))))

    (print diceValue)

    (let ((openingResult (playOpening diceValue)))
      (cond 
        ;; Check if game was won in the opening.
	((string-equal openingResult "Win") t)

	;; Check if game was lost in the opening.
	((string-equal openingResult "Loss") NIL)
	
        ;; Else, play point using the diceValue.
        (t (let ((pointResult (playPoint diceValue)))
	     (cond
	       ;; Check if game was won in point.
	       ((string-equal pointResult "Win") t)

	       ;; Check if game was loss in point.
	       ((string-equal pointResult "Loss") NIL))))))))

(defun playOpening (diceValue)
  "Plays the opening part of craps.
  Retruns Win or Loss. Nil if point needs to be played."
  ;; Check for opening loss.
    (if (member diceValue '(2 3 12))
      "Loss"

      ;; Check for opening win.
      (if (member diceValue '(7 11))
	"Win"

	;; Point needs to be played.
	NIL)))

(defun playPoint (pointValue)
  "Plays the point part of craps. Always returns Win or Loss and
  the list of dice values."
  (let ((diceValue
	  (sum (rollDice)))
	(hasResult NIL)
	(pointResult NIL))

    ;; Keep rolling until a win (pointValue) or a loss (7).
    (loop while (eq hasResult NIL) do

	  (print diceValue)

	  ;; Player wins if diceValue matches pointValue.
	  (if (= diceValue pointValue)
	    (progn
	      (setq hasResult t)
	      (setq pointResult "Win"))

	    ;; Player loses if diceValue matches 7.
	    (if (= diceValue 7)
	      (progn
	        (setq hasResult t)
		(setq pointResult "Loss"))

	      ;; Undecided, so re-roll the die.
	      (setq diceValue
		    (sum (rollDice))))))
    
    pointResult))

(print (playRound))
