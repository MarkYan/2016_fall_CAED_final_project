(defun STD-SLEEP (secs / endt)
	(setq endt (+ (getvar "DATE") (/ secs 86400.0)))
	(while (< (getvar "DATE") endt) T)
)

; random number
(defun rnd (/ modulus multiplier increment random)
  (if (not seed)
    (setq seed (getvar "DATE"))
  )
  (setq modulus    65536
        multiplier 25173
        increment  13849
        seed  (rem (+ (* multiplier seed) increment) modulus)
        random     (/ seed modulus)
  )
)

(defun round ( n ) (fix (+ n (if (minusp n) -0.5 0.5))))

(defun initialize (/)
	(graphscr) (command "osnap" "none")
	(command "ucs" "w")
	(command "vpoint" "1,-1,1")
	(command "layer" "s" "0" "")
	(command "regen")
)

(defun create_layer (/)
	(command "layer" "m" "black" "color" "blue" "" "" "" "" "")
	(command "layer" "m" "white" "color" "red" "" "" "" "" "")
	(command "layer" "m" "board" "color" "yellow" "" "" "" "" "")
	(command "layer" "m" "grave" "color" "green" "" "" "" "" "")
	(command "layer" "OFF" "grave" "Y" "")
	(command "layer" "s" "0" "")
)

(defun set_view (/)
	(command "ucs" "w")
	(command "vscurrent" "R") ;solid
	(command "vpoint" "1,-1,1")
	(command "zoom" "E" "zoom" "0.8x")
	(command "grid" "off")
)

;input code and return 3D position
(defun get_position (pos /)
	(last (assoc pos BOARD))
)

(defun rev_assoc (obj lis / i tmp)
	(setq tmp nil)
	(foreach i lis (if (= obj (nth 1 i)) (setq tmp i)))
	(princ tmp)
)

(defun get_name_by_ent (player ent /)
	(if (= player 0)
		(car (rev_assoc ent white_ents))
		(car (rev_assoc ent black_ents))
	)
)

(defun get_name_by_pos (player pos /)
	(if (= player 0)
		(car (rev_assoc pos white_pos))
		(car (rev_assoc pos black_pos))
	)
)

(defun get_pos_by_name (player _name /)
	(if (= player 0)
		(last (assoc _name white_pos))
		(last (assoc _name black_pos))
	)
)

(defun get_ent_by_name (player _name /)
	(if (= player 0)
		(last (assoc _name white_ents))
		(last (assoc _name black_ents))
	)
)

(defun set_pos_by_name (player _name pos /)
	(if (= player 0)
		(setq white_pos (subst (list _name pos) (assoc _name white_pos) white_pos))
		(setq black_pos (subst (list _name pos) (assoc _name black_pos) black_pos))
	)
)

(defun init_constant (/)
	(setq 	IP '(0.0 0.0 0.0)
			SPBoard '(8.65 0.0 -2.3)
			TOTAL_PIECES 32
	)
	(setq	SETDIR "./chess-set-7.snapshot.1"
		    FILELIST '(("board" "Chess Assembly.iam") 
		    		   ("k" "King.ipt") ("q" "Queen.ipt")
		    		   ("br" "Bishop.ipt") ("bl" "Bishop.ipt")
		    		   ("nr" "Horse 002.ipt") ("nl" "Horse 002.ipt")
		    		   ("cr" "Castle.ipt") ("cl" "Castle.ipt")
		    		   ("p1" "Pawn.ipt") ("p2" "Pawn.ipt") ("p3" "Pawn.ipt") ("p4" "Pawn.ipt")
		    		   ("p5" "Pawn.ipt") ("p6" "Pawn.ipt") ("p7" "Pawn.ipt") ("p8" "Pawn.ipt"))
	)
	(setq BOARD '(("grave" (250 250 0)) ("a8" (-154 154 0)) ("b8" (-110 154 0)) ("c8" (-66 154 0)) ("d8" (-22 154 0)) ("e8" (22 154 0)) ("f8" (66 154 0)) ("g8" (110 154 0)) ("h8" (154 154 0)) ("a7" (-154 110 0)) ("b7" (-110 110 0)) ("c7" (-66 110 0)) ("d7" (-22 110 0)) ("e7" (22 110 0)) ("f7" (66 110 0)) ("g7" (110 110 0)) ("h7" (154 110 0)) ("a6" (-154 66 0)) ("b6" (-110 66 0)) ("c6" (-66 66 0)) ("d6" (-22 66 0)) ("e6" (22 66 0)) ("f6" (66 66 0)) ("g6" (110 66 0)) ("h6" (154 66 0)) ("a5" (-154 22 0)) ("b5" (-110 22 0)) ("c5" (-66 22 0)) ("d5" (-22 22 0)) ("e5" (22 22 0)) ("f5" (66 22 0)) ("g5" (110 22 0)) ("h5" (154 22 0)) ("a4" (-154 -22 0)) ("b4" (-110 -22 0)) ("c4" (-66 -22 0)) ("d4" (-22 -22 0)) ("e4" (22 -22 0)) ("f4" (66 -22 0)) ("g4" (110 -22 0)) ("h4" (154 -22 0)) ("a3" (-154 -66 0)) ("b3" (-110 -66 0)) ("c3" (-66 -66 0)) ("d3" (-22 -66 0)) ("e3" (22 -66 0)) ("f3" (66 -66 0)) ("g3" (110 -66 0)) ("h3" (154 -66 0)) ("a2" (-154 -110 0)) ("b2" (-110 -110 0)) ("c2" (-66 -110 0)) ("d2" (-22 -110 0)) ("e2" (22 -110 0)) ("f2" (66 -110 0)) ("g2" (110 -110 0)) ("h2" (154 -110 0)) ("a1" (-154 -154 0)) ("b1" (-110 -154 0)) ("c1" (-66 -154 0)) ("d1" (-22 -154 0)) ("e1" (22 -154 0)) ("f1" (66 -154 0)) ("g1" (110 -154 0)) ("h1" (154 -154 0)) ))
	(setq white_Ipos '(("k" "e1") ("q" "d1") ("br" "f1") ("bl" "c1") ("nr" "g1") ("nl" "b1") ("cr" "h1") ("cl" "a1") ("p1" "a2") ("p2" "b2") ("p3" "c2") ("p4" "d2") ("p5" "e2") ("p6" "f2") ("p7" "g2") ("p8" "h2") ))
	(setq black_Ipos '(("k" "e8") ("q" "d8") ("br" "f8") ("bl" "c8") ("nr" "g8") ("nl" "b8") ("cr" "h8") ("cl" "a8") ("p1" "a7") ("p2" "b7") ("p3" "c7") ("p4" "d7") ("p5" "e7") ("p6" "f7") ("p7" "g7") ("p8" "h7") ))
)

(defun init_global_var (/)
	(setq white_pos '(("k" "e1") ("q" "d1") ("br" "f1") ("bl" "c1") ("nr" "g1") ("nl" "b1") ("cr" "h1") ("cl" "a1") ("p1" "a2") ("p2" "b2") ("p3" "c2") ("p4" "d2") ("p5" "e2") ("p6" "f2") ("p7" "g2") ("p8" "h2") ))
	(setq black_pos '(("k" "e8") ("q" "d8") ("br" "f8") ("bl" "c8") ("nr" "g8") ("nl" "b8") ("cr" "h8") ("cl" "a8") ("p1" "a7") ("p2" "b7") ("p3" "c7") ("p4" "d7") ("p5" "e7") ("p6" "f7") ("p7" "g7") ("p8" "h7") ))
	(setq white_ents '(("k" "tmp") ("q" "tmp") ("br" "tmp") ("bl" "tmp") ("nr" "tmp") ("nl" "tmp") ("cr" "tmp") ("cl" "tmp") ("p1" "tmp") ("p2" "tmp") ("p3" "tmp") ("p4" "tmp") ("p5" "tmp") ("p6" "tmp") ("p7" "tmp") ("p8" "tmp") ))
	(setq black_ents '(("k" "tmp") ("q" "tmp") ("br" "tmp") ("bl" "tmp") ("nr" "tmp") ("nl" "tmp") ("cr" "tmp") ("cl" "tmp") ("p1" "tmp") ("p2" "tmp") ("p3" "tmp") ("p4" "tmp") ("p5" "tmp") ("p6" "tmp") ("p7" "tmp") ("p8" "tmp") ))
)

(defun mvlyr (ent lyrname / elist)
	(setq elist (entget ent))
	(entmod (subst (cons 8 lyrname) (assoc 8 elist) elist))
)

(defun change_player (player /)
	(if (= player 0) 1 0)
)

(defun load_pieces (player _name filePath pos / ipt tmpEnt)
	(if (setq ipt (findfile filePath))
		(command "import" ipt)
	)
	(setq tmpEnt (entlast))
	(if (or (= _name "nr") (= _name "nl"))
		(command "move" tmpEnt "" IP '(0 0 11));if case
		(progn
			(command "ucs" "y" "90")
			(command "rotate" tmpEnt "" IP "90")
			(command "ucs" "w")
		)
	)
	; rotate facing
	(cond
		((= player 0) (command "rotate" tmpEnt "" IP "90"))
		((= player 1) (command "rotate" tmpEnt "" IP "270"))
	)
	(if (= player "board") 
		(command "move" tmpEnt "" IP SPBoard);if case
		(command "move" tmpEnt "" IP (get_position pos)); else case
	)

	;set layer
	(cond
		((= player 0) (mvlyr tmpEnt "white"))
		((= player 1) (mvlyr tmpEnt "black"))
		(mvlyr tmpEnt "board")
	)

	(cond
		((= player 0) (setq white_ents (subst (list _name tmpEnt) (assoc _name white_ents) white_ents)))
		((= player 1) (setq black_ents (subst (list _name tmpEnt) (assoc _name black_ents) black_ents)))
	)	
)

(defun move_piece_a (player ent start end / div _name interval maxHigh maxAngle sx sy sz xStep yStep zStep angStep x y z a b)
  	; (if (= player 0)
		; (progn
			; (command "vpoint" "0,-1,0.2")
		; )
		; (progn
			; (command "vpoint" "0,1,0.2")
		; )
	; )
	; (command "zoom" "O" ent "" "zoom" "0.2x")
	; (command "regen")
	; add moving animation here
	(setq interval 10
		  maxHigh  0
		  div 6
		  ;maxAngle 40
		  sx (car (get_position start))
		  sy (nth 1 (get_position start))
		  sz 0.0
		  ex (car (get_position end))
		  ey (nth 1 (get_position end))
		  interval (round (/ (distance (get_position start) (get_position end)) div))
		  xStep (/ (- ex sx) (float interval))
		  yStep (/ (- ey sy) (float interval))
		  zStep (/ maxHigh (/ interval 2.0))
		  ; angStep (/ maxAngle (/ maxAngle 2.0))
		  x sx y sy z sz
		  a (list x y z)
	)
  	(setq i 0)
	(repeat interval
		(STD-SLEEP 0.08)
		(if (< i (/ interval 2.0)) 
			(progn 
				(setq z (+ z zStep))
			);end if case 
			(progn
				(setq z (- z zStep))
			)
		)
		(setq x (+ x xStep)
			  y (+ y yStep)
			  b (list x y z)
		)
		(command "move" ent "" a b)
		(command "regen")
		(setq a b)
	  	(setq i (1+ i))
	)
	(setq _name (get_name_by_ent player ent))
	(set_pos_by_name player _name end)
  	(STD-SLEEP 1.0)
)

(defun move_piece_p (player ent start end / a b _name interval maxAngle maxHigh xLen yLen sx sy sz ex ey ez epos xStep yStep angStep x y z)
	(setq interval 10
		  maxHigh  20
		  ; maxAngle 90
		  sx (car (get_position start))
		  sy (nth 1 (get_position start))
		  sz 0
		  ex (nth 0 end)
		  ey (nth 1 end)
		  ez (nth 2 end)
		  xStep (/ (- ex sx) (float interval))
		  yStep (/ (- ey sy) (float interval))
		  zStep (/ (- ez sz) (float interval))
		  ;angStep (/ maxAngle (/ maxAngle 2))
		  x sx y sy z sz
		  a (list x y z)
	)
	(repeat interval
		(progn 
			(STD-SLEEP 0.0)
			(setq z (+ z zStep)
				  x (+ x xStep)
				  y (+ y yStep)
				  b (list x y z)
			)
			(command "move" ent "" a b)
			(command "regen")
			(setq a b)
			; rotate here
			; (command "ucs" "3p" a (list (1+ x) y z) (list x (1+ y) z))
			; (command "ucs" "y" "90")
			; (command "rotate" ent "" a angStep)
			; (command "ucs" "w")
		)
	)
	(STD-SLEEP 0.5)
	(princ b)
	(if (/= b end) (princ "=========> error\n")) 
)

(defun move_piece_g (player ent start / _name)
	; (command "ucs" "3p" start (list (1+ x) y z) (list x (1+ y) z))
	; (command "ucs" "y" "90")
	; (command "rotate" ent "" epos (- 0 maxAngle))
	; (command "ucs" "w")
	(mvlyr ent "grave")
	(command "regen")
	(command "move" ent "" start (get_position "grave"))
	(command "regen")
	(setq _name (get_name_by_ent player ent))
	(set_pos_by_name player _name "grave")
)

(defun move_piece (player ent start end / name)
	(command "move" ent "" (get_position start) (get_position end))
	(command "regen")
	(setq name (get_name_by_ent player ent))
	(set_pos_by_name player name end)
	(command "regen")
)

; load chess
(defun c:lc (/ boardPath player j name pos path)
	(command "erase" "all" "")
	(create_layer)
	(initialize)
	(init_constant)
	(init_global_var)

	; add board
	(setq boardPath (strcat SETDIR "/" (last (assoc "board" FILELIST)) ))
	(load_pieces "board" "board" boardPath SPBoard)

	(foreach player '(0 1) ;0 is white, 1 is black
		(setq j 0)
		(repeat (/ TOTAL_PIECES 2)
			; add all pieces
			(if (= player 0)
				(setq name (nth 0 (nth j white_Ipos))
					  pos (nth 1 (nth j white_Ipos))
				)
				(setq name (nth 0 (nth j black_Ipos))
					  pos (nth 1 (nth j black_Ipos))
				)
			)
			(setq path (strcat SETDIR "/" (last (assoc name FILELIST))))
			(load_pieces player name path pos)
			; next
			(setq j (+ j 1))
		)
	)
	(set_view)
	(princ)
)

; reset chess
(defun c:rc (/ player j _name ipos pos ent)
	(initialize)
	(foreach player '(0 1) ;0 is white, 1 is black
		(setq j 0)
		(repeat (/ TOTAL_PIECES 2)
			; add all pieces
			(if (= player 0)
				(setq _name (nth 0 (nth j white_Ipos))
					  ipos (nth 1 (nth j white_Ipos))
					  pos (nth 1 (nth j white_pos))
					  ent (get_ent_by_name player _name)
				)
				(setq _name (nth 0 (nth j black_Ipos))
					  ipos (nth 1 (nth j black_Ipos))
					  pos (nth 1 (nth j black_pos))
					  ent (get_ent_by_name player _name)
				)
			)
			(move_piece player ent pos ipos)
			;set layer
			(cond
				((= player 0) (mvlyr ent "white"))
				((= player 1) (mvlyr ent "black"))
				(princ "\nReset layer error\n")
			)
			; next
			(setq j (+ j 1))
		)
	)
	(set_view)
	(princ)
)

(defun get_tmp_pos (spos / ret)
	(setq pos (get_position spos)
			x (nth 0 pos)
			y (nth 1 pos)
			z (nth 2 pos)
			x (+ x 30)
			; y (+ y (fix(* 30 (rnd))))
			;z (+ z (fix(* 20 (rnd))))
			ret (list x y z)
	)
	(princ ret)
)

; start playing game
(defun c:stg (/ cmdType st end ent name flag turns cmd mode)
;	(c:rc)
	(setvar "cmdecho" 0)
  	(setq cmd (getstring "\nSelect game mode <p2p>: "))
	(if (= cmd "load") (setq mode 1) (setq mode 0)) ;mode0: p2p, mode1: load from file

	; open file
	(if (= mode 1) (setq fp (open "d:/A2015/game1.txt" "r")))

	(setq flag 1)
	(setq turns 0) ;0 is white, 1 is black
	(while (= flag 1)
		(if (= mode 1) (STD-SLEEP 0.5)) ;delay if input from file
	  	; get input
	  	(if (= mode 1) 
	  		;if
			(setq cmd (read-line fp))
			; else
			(if (= turns 0)
				(progn
					(command "vpoint" "0,-1,1.5")
					(setq cmd (getstring "\nPlayer white : "))
				)
				(progn
					(progn
						(command "vpoint" "0,1,1.5")
						(setq cmd (getstring "\nPlayer black : "))
					)	
				)
			)
		)
		
	  	;parse cmd
		(setq cmdType (substr cmd 1 1))
		(cond
			((= cmdType "0") ;castling
				(princ "castling")
			)
			((= cmdType "-") ;end-of-game
				(setq flag 0)
			)
			(progn
				(setq cmdType (substr cmd 3 1))
			 	(setq st (substr cmd 1 2)
				      end (substr cmd 4 2)
				      name (get_name_by_pos turns st)
				      ent  (get_ent_by_name turns name)
				)
			 	(if (= cmdType "x")
					(setq namek (get_name_by_pos (change_player turns) end)
					      entk (get_ent_by_name (change_player turns) namek)
						  tmp_pos (get_tmp_pos end)
					)
				)
				;kill
				(if (= cmdType "x") 
					(move_piece_p (change_player turns) entk end tmp_pos)
				)
				;moving
				(move_piece_a turns ent st end)
				;kill
				(if (= cmdType "x") 
					(move_piece_g (change_player turns) entk tmp_pos)
				)
			)
		);end cond
		(if (/= mode 1) 
			(progn
				(STD-SLEEP 0.5)
				(command "vpoint" "1,-1,1")
				(command "regen")
				(STD-SLEEP 1)
			)
		)
		(setq turns (change_player turns))
	);end while
	(princ)
)

(defun c:show (/)
	(princ "\n ====== DEBUG =====\n")
	; (princ BOARD)
	(princ "\n white_pos: \n")
	(princ white_pos)
	; (princ "\n white_ents: \n")
	; (princ white_ents)
	(princ "\n black_pos: \n")
	(princ black_pos)
	; (princ "\n black_ents: \n")
	; (princ black_ents)
	(princ)
)

(defun c:test (/)
	(princ (round 1.1234856))
  	(setq fp (open "d:/A2015/123.txt" "r"))
	(setq cmd (read-line fp))
  	(princ)
)