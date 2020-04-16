; Vinh Trang
; 11/11/2019
; HW05


(setq *x* 4)
(setq *y* 4)
(setq *connect* 3)
(setq *item* '("X" "O" " "))
(setq *search* 4)

; create board with dimension x*y
(defun createboard()
	(let (bd)
		(dotimes (n1 *x* bd)
			(setq bd (cons (let (c)
				(dotimes (n2 *y* c)
					(setq c (cons 2 c))))bd)))))
					
;print out current board
(defun printboard (board)
    (let ((space "   "))
        (terpri)
        (dotimes (i *y*)
            (princ space)(princ "|")
            (dolist (c board)
                (princ " ")
                (princ (nth (nth (- (1- *y*) i) c) *item*))
                (princ " |"))
            (terpri)
            (princ space)(princ "|")
            (dolist (col board)
                (princ "___|"))
            (terpri))
			(princ space)
			(dotimes (f *x*)
				(princ " c")
				(princ (1+ f))
				(princ " "))
        (terpri) (terpri)))
		
		
(defun initiate()
	(setq *bmap* (findmap))
	(setq *bsize* (* *x* *y*))
	(setq *win* (expt 2 *connect*))
	(setq *randomseed* (make-random-state t)))
	
	
(defun possiple-wins-pos()
;x and y is dimension of board and we need to w = connect 3 to wins
; The solution for possible win positions is 4xy -3xw -3yw +3x +3y -4w +2w^2 +2
	(+ (* 4 *x* *y*)
		(-(* 3 *x* *connect*))
		(-(* 3 *y* *connect*))
		(* 3 *x*)(* 3 *y*)
		(-(* 4 *connect*))
		(* 2 *connect* *connect*)
		2))
	
(defun findmap()

	;make empty map first
	(let (map)
		(dotimes (n1 *x*)
			(setq map (cons (let (c)
				(dotimes (n2 *y* c)
					(setq c (cons nil c)))) map)))
	;createmap
	(let ((temp 0))
		
		;horizontal wins
		(dotimes (n1 (1+ (- *y* *connect*)))
			(dolist (col map)
				(let((cp(nthcdr n1 col)))
					(dotimes (n2 *connect*)
						(setf (nth n2 cp)
							(cons temp (nth n2 cp))))
					(setq temp (1+ temp)))))
		;verticle wins
		(dotimes (n1 (1+ (- *x* *connect*)))
				(let((bp(nthcdr n1 map)))
					(dotimes (n2 *y*)
						(dotimes (n3 *connect*)
							(setf (nth n2 (nth n3 bp))
								(cons temp (nth n2 (nth n3 bp)))))
						(setq temp (1+ temp)))))
		;diagonal wins
		(dotimes (n1 (1+ (- *y* *connect*)))
			(dotimes (n2 (1+ (- *x* *connect*)))
					(let((bp(nthcdr n2 map)))
						(dotimes (n3 *connect*)
							(setf (nth (+ n3 n1) (nth n3 bp))
								(cons temp
									(nth (+ n3 n1) (nth n3 bp)))))
						(setq temp (1+ temp)))))	
								
		(dotimes (n1 (1+ (- *y* *connect*)))
			(dotimes (n2 (1+ (- *x* *connect*)))
				(let((bp(nthcdr n2 map)))
					(dotimes (n3 *connect*)
						(setf (nth (- *y* (+ n3 n1) 1)
												(nth n3 bp))
                            (cons temp 
								(nth (- *y* (+ n3 n1) 1)
												(nth n3 bp)))))
                        (setq temp (1+ temp)))))
            (if (/= temp (possiple-wins-pos))
				(error "error getting map")))
        map))
		
(defun other (player) (- 1 player))
		
;find out nextstage if it is com or player		
(defun nextstate (state) 
	(setf (nth 2 state) (other (nextstageof state))))
	
	
;call printboard using chosing state
(defun printboardof (state)
	(printboard (boardof state)))
	
;creategamestate
(defun creategamestate()
	(list (createboard) (list (createstat) (createstat)) 0 0))
	
;create new stat
(defun createstat ()
	(let(stat)
		(dotimes (n (possiple-wins-pos)stat)
			(setq stat (cons 1 stat)))))
	
	
; usage (connect-3) to start
(defun connect-3()
	(initiate)
		(let ((state (creategamestate)))
			(princ "Would you like to play as x(0) or O(1)? ")
			(let ((input (read)))
				(if (and (numberp input) (= 1 input))
					(nextstate state)))
			(princ "Would you like to fo first? yes(0) no(1) ")
			(let ((input (read)))
				(cond
					((and (numberp input) (= 1 input))
						(nextstate state)	
						(printboardof state)
						(compturn state))))
						(game state)))
					
;Player vs computer game		
(defun game (state)
	(printboardof state)
	(if (askplayer state)
		(cond
			((iswin (other (nextstageof state)) state)
				(printboardof state)
				(princ "You won!") (terpri)
				t)
			((isdraw state)
				(printboardof state)
				(princ "Gameover, It is a draw!") (terpri)
				t)
			(t
				(printboardof state)
				(compturn state)
				(cond
					((iswin (other(nextstageof state))state)
						(printboardof state)
						(princ "Computer won!") 
						(terpri) t)
					((isdraw state)
						(printboardof state)
						(princ "Gameover, It is a draw!") 
						(terpri)t)
					(t
						(game state)))))))
(defun iswin (player state)
	(if (member *win* (statof player state)) t nil))

(defun isdraw (state)
	(if (= (itemof state) *bsize*) t nil))
	
;computer interaction with user			
(defun compturn (state)
	(terpri)
	(let ((move (minimax state)))
		(makemove move state)
		(princ "Computer use piece '")
		(princ (nth (other(nextstageof state)) *item*))
		(princ "' on column ")
		(princ move) (terpri)))

; choose min value in list if we have matching min choose random
(defun minposition (list)
	(let ((minn (eval (cons 'min list))) positions)
		(dotimes (pos (length list))
			(if (= (nth pos list) minn)
				(setq positions(cons pos positions))))
		(nth (random (length positions) *randomseed*) positions)))
		
;minimax for computer process best next move
(defun minimax (state)
	(1+ (minposition (reverse (evallistof state 0)))))

;return list of use searchaheadof to eval how bad after nextmove is
(defun evallistof (state depth)
	(let(evallist state0)
		(dotimes (c *x*)
			(setq state0 (deeplist state))
			(if (makemove (1+ c) state0)
				(setq evallist (cons (-
					(searchaheadof state0 (1+ depth))) evallist))
				(setq evallist (cons 2000 evallist))))
			(mapcar #'(lambda (x) (if (<= x -950) (1+ x) x)) evallist)))
			
; return a deep level of list
(defun deeplist (list)
	(let (templist)
		(dolist (e list (reverse templist))
			(setq templist (cons	
				(if (listp e) (deeplist e) e) templist)))))
			
;return how bad state is for current stat by looking into deeper level *search* value can be change
(defun searchaheadof (state depth)
	(if (iswin (other (nextstageof state)) state)
		1000
		(if (>= depth *search*)
			(staticof (other (nextstageof state)) state)
				(minoflist (evallistof state depth)))))
				
;minimum value of list
(defun minoflist (list)
    (eval (cons 'min list)))
		
;user interaction
(defun askplayer (state)
	(princ "choose the column you want to play: ")
	(let ((input (read)))
		(case input
            ((0 q Q)
                (princ "Do you want to quit? ")
                (case (read)
                    ((y Y) nil)
                    (t (query-player state))))
            (t
				(if (and (numberp input) (makemove input state))
					t
					(askplayer state))))))
			
;makemove function to drop piece down to chosing column
(defun makemove (input state)
	(if (> input *x*)
		nil
			(let ((column (nth (1- input) (boardof state))))
				(let ((slot (member 2 column)) (player (nextstageof state)))
					(cond ((not slot) nil) (t
					; move piece in slot.
					(setf (car slot) player)
					; Change player's stats to reflect move.
					(dolist (winslot (nth (- *y* (length slot))
								(nth (1- input) *bmap*)))
						(setf (nth winslot (statof player state))
							(* (nth winslot (statof player state)) 2))
						(setf (nth winslot (statof (other player) state)) 0))
					(setf (nth 3 state) (1+ (itemof state)))
                    (nextstate state)
                    t))))))

;eval how good a state of player					
(defun staticof (player state)
	(- (eval (cons '+ (statof player state)))
		(eval (cons '+ (statof (other player) state)))))
					
;different stage of state		
(defun boardof (state) (first state))				
(defun statof (player state) (nth player (second state)))				
(defun nextstageof (state) (third state))
(defun itemof (state) (fourth state))
						
(princ "Usage (connect-3) to start the program! ")
			
			
		
					
					

					
			

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	