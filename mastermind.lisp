; Assignment 3
; Mastermind in Lisp
; Michael O'Ryan 4612450
(defun mastermind ()
  (labels
   (

    ; Count position matches
    (countposmatches(code_x code_y) 
		    (reduce #'+ 
			    (
                             ; Count number of matches
			     (lambda (code_x code_y)
			       (mapcar 
		                ; Convert x to a 1 or 0
				#'(lambda (b) (cond (b 1) (t 0))) 
				; Position Matches
				((lambda (code_x code_y)
				   (mapcar
			             ; x = y comparison for mapcar
				    (lambda (x y) (= x y))
				    code_x code_y
				    )
				   )
				 code_x code_y) 
				)
			       )
			     code_x code_y
			     )
			    )
		    )
    ; Count colours in a list
    ; Iterate over list and count each colour found
    ; store in array
    ; [1 0 3 0]
    ; 1, 0 colour
    ; 0, 1 colours
    ; 3, 2 colours
    ; 0, 4 colours
    ; return array
    (countnums(code arr) 
	      (cond
	       ( (equal code '()) arr)
	       (t (
		   ; Add colour to array
		   (lambda(x arr)
		     (setf (aref arr x)
			   (+ 1 (aref arr x))
			   )
		     arr
		     )
		   (car code) (countnums (cdr code) arr)
		   )
		  )
	       )
	      )

    ; Count Colours
    (colourscount(a b colours)
	         ;sum
		 (reduce #'+ 
			 (mapcar #'min 
				 (coerce
				  (countnums a (make-array colours :initial-element 0) ) 'list)
				 (coerce
				  (countnums b (make-array colours :initial-element 0) ) 'list)
				 )
			 )
   
		 )

    ; Position and colour matches
    ; Return (positions colours)
    (matches (colours guess answer)
	     (list (countposmatches guess answer)  (- (colourscount guess answer colours) (countposmatches guess answer)))
	     )


    ; state (current prev size first)
    ; QQ I need to do a loop here unless I can think of a better way :'(
    (findnext (state)


	      (labels
	       (
	        ; Generate the next code in sequence
  	        ; current = current code
	        ; highest = base
         	; in base 3 (1 2 1 1) -> (2 2 1 1) -> (0 0 2 1) -> etc
	
		(nextcode (current highest)
			  (labels
			   (
		            ; Find next number
		            ; Loops to 0 if reaches highest (base)
		            ; Returns new number and if there is a carry/return to 0
			    (increment (number highest)
				       (cond 
					( (equal (+ number 1) highest)
					  '(0 t)
					  )
					(number (list (+ number 1) nil)
						)
					(t '(0 nil))
					)
				       )
		            ; Increment an entire list
		            ; Start with first and increment by 1
		            ; if there is a carry add it to the next position
		            ; Will not append a something to the end (2 2) base 3 incremented -> (0 0)
			    (incrementlist (lst highest carry)
					   (let (
						 (next (increment (car lst) highest)
						       )
						 )
					     (cond
					      ( (and (not (cdr lst)) carry) (list (car next)))
					      (carry (cons (car next)
							   (incrementlist (cdr lst) highest (car (cdr next))
									  )
					   
							   )
						     ) 
					      (t lst)
					      )
					     )
					   )
			    )
	   
			   (incrementlist current highest t)
			   )
			  )
	
		; Guess (c1 c2 c3 ... cn)
		; prev ((pos colour) (c1 c2 c3 .. cn))
		; size (positions colours)
		(checkprev (guess prev size)
			   (cond
			    ((or (equal prev nil) (equal prev '(())) ) guess)
			    ( (equal (matches (cadr size) (cadar prev) guess) (caar prev)) (checkprev guess (cdr prev) size))
			    (t nil)
			    )
			   )	
	
		)

	       (loop
		(when (equal (nextcode (car state) (car (cdaddr state)) ) (cadddr state)) (return nil))
		(when (checkprev (nextcode (car state) (car (cdaddr state))) (cadr state) (caddr state) ) (return (nextcode (car state) (car (cdaddr state)) )))
		(setq state (cons (nextcode (car state) (car (cdaddr state))  ) (cdr state)))
		)
	       )
	      )




    ;size (positions colours)
    (randomcode (size)
		(labels
		 (
		  (randomlist (size max_n)
			      (cond 
			       ((equal size 0) '())
			       (t (cons (random max_n) (randomlist (- size 1) max_n)))
			       )
			      )
		  )
		 (randomlist (car size) (cadr size))
		 )
		)

    ; low lowest number; n number; high highest number
    ; returns true or nil
    (between (low n high)
	     (cond
	      ((and (<= low n) (>= high n) ) t)
	      (t nil)
	      )
	     )



    ; Asks user to enter size of game
    ; return (positions colours)
    (getsize ()
	     (labels 
	      (
	       ; size (positions colours)
	       ; Verifies that a size is valid
	       ; return (positions colours)
	       (verifygamesize (size)
			       (cond
				( (not (and (typep (car size) 'integer) (typep (cadr size) 'integer) ))
				  (format t "Incorrect game size.")
				  (getsize) )

				( (and (between 2 (car size) 8) (between 2 (cadr size) 8)) size)
				(t (format t "Incorrect game size.")
				   (getsize) )
				)
			       )
	       )
	
	      (format t "~%Enter number of Positions then Colours. Min 2 Max 8:")
	      (verifygamesize (list (read) (read)))
	      )
	     )

    ; colourcount number of different colours
    ; current current colour to print, start at 0
    (printpossible (colourcount current)
		   (labels (
			     ; Comma or And
			     ; Returns and or , depending on whether one should be included in a sentence. one, two, three and four
			     ; last number of items
			     ; cur current item in list
			     ; will return and when last = cur + 1
			     ; other wise ,
			    (commaorand (last cur)
					(cond ( (equal (- last 1) cur ) " and ")
					      ( (equal cur 0) "")
					      (t ", ")
					      )
					)
			    )

			   (cond 
			    ((> colourcount current)
			     (format t (commaorand colourcount current))
			     (printcolour (maptocolour current) (numbertocolourcode current))
			     (printpossible colourcount (+ current 1) ) )
			    (t "")
			    )
			   )
		   )
     ; State ( (position colour) (c1 c2 c3 ... cn) )
    (showpossible (state)
		  (labels (
			   ; State ( (position colour) (c1 c2 c3 ... cn) )
			   (getnumcolours (state)
					  (cadar state)
					  )
			   )

			  (format t "Possible colour~p are " (getnumcolours state))
			  (printpossible (cadar state) 0)
			  state
			  )
		  )


    ; Initialise a game where the player is guessing the code
    ; returns player state
    (init()
	 (labels
	  (
	   ; Generate the state of a player guessing the computers code game
           ; size (positions colours)
           ; returns ( (positions colours) (c1 c2 c3 ... cn) )
	   (genstate (size)
		     (list size (randomcode size))
		     )
	   )
	  (showpossible (genstate (getsize)) )
	  )
	 )

    ; get a guess from a player
    ; Size (positions colours)
    (getguessloop(size)
		 (cond 
		  ((equal (car size) 0) '())
		  (t (cons (read ) (getguessloop (list (- (car size) 1) (cadr size) )))) 
		  )
		 )

    ; Size (positions, colours)
    (verifyguess (lst size)
		 (labels (
			  ; if val exists in lst
			  ; lst (a, b, c, d, e, f)
			  (exists (lst val)
				  (cond 
				   ( (equal lst nil) nil)
				   ( (equal (car lst) val) t)
				   (t (exists (cdr lst) val))
				   )
				  )
			  )

			 (cond
			  ( (or (exists lst nil) (not (equal (list-length lst) (car size) ) )) (format t "Incorrect guess. Please enter ~r position~p out of these possible colour~p.~%" (car size) (car size) (cadr size) ) (printpossible (cadr size) 0)  (getguess size))
			  (t lst)
			  )
			 )
		 )


    ;Size (positions, colours)
    (getguess(size)
	     (labels
	      (
               ; Print |rgby|rky
               ; as guide for player to know number of positions and colour code options
               ; size (positions colours)
	       (playerguessheader (size count currentcolour)
				  (cond
				   ( (and (>= (car size) (cadr size)) (> count (car size))) nil)
				   ( (and (< (car size) (cadr size)) (>= currentcolour (cadr size))) nil)
				   ( (equal count 0) (format t "|") (printcolour (maptocolour currentcolour) (numbertocolourletter currentcolour)) (playerguessheader size (+ count 1) (+ currentcolour 1)))
				   ( (>= currentcolour (cadr size)) (playerguessheader size count 0))
				   ( (and (equal count (car size)) (<= (cadr size) (car size) ))  (format t "|") (playerguessheader size (+ count 1) (+ currentcolour 1)))
				   ( (equal count (car size))  (format t "|") (printcolour (maptocolour currentcolour) (numbertocolourletter currentcolour)) (playerguessheader size (+ count 1) (+ currentcolour 1)))
				   (t (printcolour (maptocolour currentcolour) (numbertocolourletter currentcolour)) (playerguessheader size (+ count 1) (+ currentcolour 1)))
				   )
				  )
	       )
	      (format t "~%                ")
	      (playerguessheader size 0 0)
	      (format t "~%Enter your guess:")
	      (verifyguess (map 'list #'lettertonumber (remove #\Space (listify (read-line) 0)) ) size)
	      )
	     )

    ; return "" if n = 1
    ; return "es" if n != 1
    (plurales (n)
	      (cond 
	       ((equal n 1) "")
	       (t "es")  
	       )
	      )


    ; State ( (position colour) (c1 c2 c3 c4... cn) )
    (playgame(state guesses)
	     (labels 
	      (
               ; correct (positions colours)
	       (printcorrect (correct)
			     (format t "~&~@(~r~) position~p and ~r colour~p matched." (car correct) (car correct) (cadr correct)  (cadr correct))
			     correct
			     )

               ; matches number of position matches in guess
               ; positions number of positions in game
	       (haswon (matches positions)
		       (equal positions (car matches))
		       )
	       ; Player wins
               ; Congratulate and tell number of guesses taken
	       (dowin (guesses)


		      (format t "~%You won!")
		      (format t "~%You took ~r guess~a" guesses (plurales guesses))
		      )
               ; count of positions and colours
	       (poscolourcount(a b colours)
			      (list (countposmatches a b)  (- (colourscount a b colours) (countposmatches a b)))
			      )
	       )

	      (cond 
	       ( (haswon (printcorrect (poscolourcount (getguess (car state)) (cadr state) (cadar state) ) ) (caar state )) (dowin guesses) )
	       (t (playgame state (+ guesses 1)) )
	       )
	      )
	     )

    ; Player input to colour code
    (lettertonumber (letter)
		    (cond
		     ( (equal (char-upcase letter) #\R) 0)
		     ( (equal (char-upcase letter) #\G) 1)
		     ( (equal (char-upcase letter) #\B) 2)
		     ( (equal (char-upcase letter) #\Y) 3)
		     ( (equal (char-upcase letter) #\P) 4)
		     ( (equal (char-upcase letter) #\W) 5)
		     ( (equal (char-upcase letter) #\K) 6)
		     ( (equal (char-upcase letter) #\D) 7)
		     ( (equal (char-upcase letter) #\E) 8)
		     (T nil)
		     )
		    )

    ; Colour code to the corresponding colour word
    (numbertocolour (n)
		    (cond
		     ((equal n 0) "Red")
		     ((equal n 1) "Green")
		     ((equal n 2) "Blue")
		     ((equal n 3) "Yellow")
		     ((equal n 4) "Purple")
		     ((equal n 5) "White")
		     ((equal n 6) "Dark Grey")
		     ((equal n 7) "Gold")
		     ((equal n 8) "Ebony")
		     (t nil)
		     )
		    )

    ; Colour code to the corresponding colour letter
    (numbertocolourletter (n)
			  (cond
			   ((equal n 0) "R")
			   ((equal n 1) "G")
			   ((equal n 2) "B")
			   ((equal n 3) "Y")
			   ((equal n 4) "P")
			   ((equal n 5) "W")
			   ((equal n 6) "K")
			   ((equal n 7) "D")
			   ((equal n 8) "E")
			   (t nil)
			   )
			  )

    ; Colour code to the corresponding colour word indicating the shortcut character
    (numbertocolourcode (n)
			(cond
			 ((equal n 0) "(R)ed")
			 ((equal n 1) "(G)reen")
			 ((equal n 2) "(B)lue")
			 ((equal n 3) "(Y)ellow")
			 ((equal n 4) "(P)urple")
			 ((equal n 5) "(W)hite")
			 ((equal n 6) "Dar(k) Grey")
			 ((equal n 7) "Gol(d)")
			 ((equal n 8) "(E)bony")
			 (t nil)
			 )
			)


    ; console colour for each colour
    (maptocolour (n)
		 (cond
		  ((equal n 0) 1)
		  ((equal n 1) 10)
		  ((equal n 2) 12)
		  ((equal n 3) 11)
		  ((equal n 4) 5)
		  ((equal n 5) 7)
		  ((equal n 6) 8)
		  ((equal n 7) 3)
		  ((equal n 8) 4)
		  (t nil)
		  )
		 )

    ; Print a string in a specified colour
    (printcolour (clr str)
		 (labels
		  (
		   ; Set the console to a specified colour
		   (setcolour (clr)
			      (format t (concatenate 'string "~c[38;5;" (write-to-string clr) "m") #\ESC)
			      )

		   ; Reset console colour to default
		   (clearcolour ()
				(format t "~c[0m" #\ESC)
				)
		   )
		  (setcolour clr)
		  (format t "~a" str)
		  (clearcolour)
		  )
		 )


    ; Generate a random list of n items with between 0 and max
    (randomlist (max n)
		(cond
		 ( (equal n 0) '())
		 ( (cons (random max) (randomlist max (- n 1))))
		 )
		)

    ; Print a string with random colours for each letter
    (printrandomcolours (str)
			(map 'nil #'printcolour  (randomlist 255 (length str)) (listify str 0))
			)


    ; Start a game where the player is guessing
    (startplayer ()
		 (playgame (init) 1)
		 )

    ; Listify a string, pos initially 0 for index 0
    ; "abc" => (#\a #\b #\c)
    (listify (str pos)
	     (cond
	      ( (typep str 'symbol) (list str))
	      ((equal pos (length str)) '())
	      (t (cons (char str pos) (listify str (+ pos 1))))
	      )
	     )

    ; Get input from user and return as a list
    ; tokenise by each character
    ; remove all spaces
    (getinput (lst size)
	      (cond 
	       ((< (list-length lst) size) (getinput (append lst (remove #\Space (listify (read-line) 0)) ) size))
	       (t lst)
	       )
	      )

    ; State (firstguess (guesses) )
    ; first guess (c1 c2 c3 ... cn)
    ; guesses (g1, g2, g3, ... gn)
    ; gn ( (positions colours) (c1, c2, c3, ... cn) )
    (compinit ()
	      (playcomp (firstguess (randomcodeandsize (getsize))))
	      )

    ; Start a game where the computer is guesses
    (startcomp ()
	       (playcomp (compinit))
	       )

    ; Print the guess a computer makes
    ; eg; red blue green yellow
    (printcompguess (guess)
		    (cond
		     ((not guess) nil)
		     (t (printcolour (maptocolour (car guess)) (numbertocolour (car guess))) (format t " ") (printcompguess (cdr guess)))
		     )
		    )

    ; Player input for number of positions and colours matched
    (playeranswer (guess size)
		  (labels
		   (
		    (verifyanswer (positions colours size)
				  (cond
				   ( (not (and (typep positions 'integer) (typep colours 'integer) ))
				     (format t "Invalid number of positions and colours. Please don't cheat.~%")
				     (format t "~%How many positions and colours did I get correct? ")
				     (verifyanswer (read) (read) size)
				     )
				   ( (and (and (between 0 positions (car size)) (between 0 colours (car size) )) (between 0 (+ positions colours) (car size)))
				     (list positions colours)
				     )
				   (t (format t "Invalid number of positions and colours. Please don't cheat.~%")
				      (format t "~%How many positions and colours did I get correct? ")
				      (verifyanswer (read) (read) size)
				      )
				   )
				  )
		    )
		   (printcompguess guess)
		   (format t "~%How many positions and colours did I get correct? ")
		   (list (verifyanswer (read) (read) size) guess)

		   )
		  )


    (compguess (state)
	       ((lambda (guess state)
		  (cond 
		   ((equal guess nil) nil)
		   (t (cons guess (cons (cons (playeranswer guess (caddr state)) (cadr state)) (cddr state))))
		   )
		  )
		(findnext state) state)
	       )



    ;size (positions colours)
    (randomcodeandsize(size)
		      ((lambda (randomcode size)
			 (list randomcode '() size randomcode)
			 ) (randomcode size) size)

		      )

    ; state (current prev size first)
    ; First guess of computer
    (firstguess (state)
		(cons (car state) (cons (cons (playeranswer (car state) (caddr state) ) (cadr state)) (cddr state)))
		)


    ; Comp wins and tells player
    (compwins (state)
	      (format t "~%I won in ~r guess~a!" (length (cadr state)) (plurales (length (cadr state))))
	      "Gameover"
	      )

    ; Player cheated. Let them know
    (playercheated ()
		   (format t "~%You cheated!")
		   "Gameover"
		   )

    ; Has the player cheated?
    (hasplayercheated (state)
		      (equal '(nil) (caadr state))
		      )

    ; Has computer guessed the players code?
    (comphaswon (state)
		(equal (car (caaadr state)) (length (car state)))
		)

    ; Computer guesses code
    (playcomp (state)
	      (cond
	       ( (equal state "Gameover") "")
	       ( (equal state nil) (playercheated) )
	       ( (comphaswon state) (compwins state))
	       (t (playcomp (compguess state) ))
	       )
	      )

    ; Does the player want to play again?
    (playagain (option)
	       (cond 
		( (equal option nil) (format t "~%Play again y/n?") (playagain (car (getinput '() 1))))
		( (equal (char-upcase option)  #\Y) (gameloop) )
		( (equal (char-upcase option)  #\N) (format t "~%Thanks for playing!"))
		(t (format t "~%Play again y/n?") (playagain (car (getinput '() 1))))
		)

	       )

    ; Game loop for running the game. Loops (recursively :P ) until player chooses to quit
    (gameloop ()
	      (labels
	       (
		; Read in y or c
		(whowillguess (guesser)
			      
			      (labels (
				       )
				      (cond
				       ((equal (char-upcase guesser) #\Y ) (startcomp) (playagain nil))
				       ((equal (char-upcase guesser) #\C ) (startplayer) (playagain nil))
				       
				       (T (format t "~%Invalid choice. Please select y for you to pick the code~%or c for the computer to pick the code: ")
					  (whowillguess (car (getinput '() 1)))
					  )
				       )
				      )

			      )

		)

	       (format t "~%How will pick the code? (Y)ou or (C)omputer? ")
	       (cond 
		((whowillguess (car (getinput '() 1)) ) (gameloop))
		(t nil)
		)
	       )
	      )

    ; Intro of mastermind
    (intro()
	  (format t "Welcome to ")
	  (printrandomcolours "Mastermind!")
	  )

    )
   (intro) 
   (gameloop)
   )
  )

; Auto start Mastermind

(mastermind)
