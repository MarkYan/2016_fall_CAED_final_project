(defun STD-SLEEP (secs / endt)
	(setq endt (+ (getvar "DATE") (/ secs 86400.0)))
	(while (< (getvar "DATE") endt) T)
) ; not varified

(defun initialize (/)
	(graphscr) (command "osnap" "none")
	(command "ucs" "w")
	(command "vpoint" "1,-1,1")
	(command "layer" "s" "0" "")
)

(defun create_layer (/)
	(command "layer" "m" "black" "color" "blue" "" "" "" "" "")
	(command "layer" "m" "white" "color" "red" "" "" "" "" "")
	(command "layer" "m" "board" "color" "yellow" "" "" "" "" "")
)

(defun set_view (/)
	(command "ucs" "w")
	(command "vscurrent" "s") ;solid
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

(defun get_pos_by_name (player name/)
	(if (= player 0)
		(last (assoc name white_pos))
		(last (assoc name black_pos))
	)
)

(defun get_ent_by_name (player name/)
	(if (= player 0)
		(last (assoc name white_ents))
		(last (assoc name black_ents))
	)
)

(defun set_pos_by_name (player name pos /)
	(if (= player 0)
		(setq white_pos (subst (list name pos) (assoc name white_pos) white_pos))
		(setq black_pos (subst (list name pos) (assoc name black_pos) black_pos))
	)
)

(defun init_constant (/)
	(setq 	IP '(0 0 0)
			SPBoard '(8.65 0 -2.3)
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
	(setq BOARD '(("GRAVE" (200 200 0)) ("a8" (-154 154 0)) ("b8" (-110 154 0)) ("c8" (-66 154 0)) ("d8" (-22 154 0)) ("e8" (22 154 0)) ("f8" (66 154 0)) ("g8" (110 154 0)) ("h8" (154 154 0)) ("a7" (-154 110 0)) ("b7" (-110 110 0)) ("c7" (-66 110 0)) ("d7" (-22 110 0)) ("e7" (22 110 0)) ("f7" (66 110 0)) ("g7" (110 110 0)) ("h7" (154 110 0)) ("a6" (-154 66 0)) ("b6" (-110 66 0)) ("c6" (-66 66 0)) ("d6" (-22 66 0)) ("e6" (22 66 0)) ("f6" (66 66 0)) ("g6" (110 66 0)) ("h6" (154 66 0)) ("a5" (-154 22 0)) ("b5" (-110 22 0)) ("c5" (-66 22 0)) ("d5" (-22 22 0)) ("e5" (22 22 0)) ("f5" (66 22 0)) ("g5" (110 22 0)) ("h5" (154 22 0)) ("a4" (-154 -22 0)) ("b4" (-110 -22 0)) ("c4" (-66 -22 0)) ("d4" (-22 -22 0)) ("e4" (22 -22 0)) ("f4" (66 -22 0)) ("g4" (110 -22 0)) ("h4" (154 -22 0)) ("a3" (-154 -66 0)) ("b3" (-110 -66 0)) ("c3" (-66 -66 0)) ("d3" (-22 -66 0)) ("e3" (22 -66 0)) ("f3" (66 -66 0)) ("g3" (110 -66 0)) ("h3" (154 -66 0)) ("a2" (-154 -110 0)) ("b2" (-110 -110 0)) ("c2" (-66 -110 0)) ("d2" (-22 -110 0)) ("e2" (22 -110 0)) ("f2" (66 -110 0)) ("g2" (110 -110 0)) ("h2" (154 -110 0)) ("a1" (-154 -154 0)) ("b1" (-110 -154 0)) ("c1" (-66 -154 0)) ("d1" (-22 -154 0)) ("e1" (22 -154 0)) ("f1" (66 -154 0)) ("g1" (110 -154 0)) ("h1" (154 -154 0)) ))
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

(defun load_pieces (player name filePath pos / ipt tmpEnt)
	(if (setq ipt (findfile filePath))
		(command "import" ipt)
	)
	(setq tmpEnt (entlast))
	(if (or (= name "nr") (= name "nl"))
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
		((= player 0) (setq white_ents (subst (list name tmpEnt) (assoc name white_ents) white_ents)))
		((= player 1) (setq black_ents (subst (list name tmpEnt) (assoc name black_ents) black_ents)))
	)	
)

(defun move_piece_a (player ent start end / name interval maxHigh maxAngle sx sy sz xStep yStep zStep angStep x y z a b)
	; add moving animation here
	(setq interval 10
		  maxHigh  20
		  maxAngle 45
		  sx (car (get_position start))
		  sy (nth 1 (get_position start))
		  sz 0
		  ex (car (get_position end))
		  ey (nth 1 (get_position end))
		  xStep (/ (- ex sx) interval)
		  yStep (/ (- ey sy) interval)
		  zStep (/ maxHigh (/ interval 2))
		  angStep (/ maxAngle (/ maxAngle 2))
		  x sx y sy z sz
		  a (list x y z)
	)
	(repeat interval
		(if (< interval (/ interval 2)) 
			(progn 
				(setq z (+ z zStep))
				; rotate here
				(if (= player 0) 
					(progn
						(command "ucs" ent "") ; can be bug
						(command "ucs" "y" "90")
						(command "rotate" ent "" a angStep)
						(command "ucs" "w")
					)
					; (progn
					; 	(command "ucs" ent "")
					; 	(command "ucs" "y" "90")
					; 	(command "rotate" ent "" a (- 0 angStep))
					; 	(command "ucs" "w")
					; )
				)
			);end if case 
			(progn
				(setq z (- z zStep))
				; rotate here
				(if (= player 0) 
					(progn
						(command "ucs" ent "")
						(command "ucs" "y" "90")
						(command "rotate" ent "" a (- 0 angStep))
						(command "ucs" "w")
					)
					; (progn
					; 	(command "ucs" ent "")
					; 	(command "ucs" "y" "90")
					; 	(command "rotate" ent "" a angStep)
					; 	(command "ucs" "w")
					; )
				)
			)
		)
		(setq x (+ x xStep)
			  y (+ y yStep)
			  b (list x y z)
		)
		(command "move" ent "" a b)
		(setq a b)
		(STD-SLEEP 5)
	)
	;(command "move" ent "" (get_position start) (get_position end))
	(setq name (get_name_by_ent player ent))
	(set_pos_by_name player name end)
)

(defun move_piece_k (player ent start end / name)
	(setq interval 10
		  maxHigh  5
		  maxAngle 95
		  xLen 30
		  yLen 30
		  sx (car (get_position start))
		  sy (nth 1 (get_position start))
		  sz 0
		  ex (+ sx xLen)
		  ey (+ sx yLen)
		  ez maxHigh
		  epos (list ex ey ez)
		  xStep (/ (- ex sx) interval)
		  yStep (/ (- ey sy) interval)
		  zStep (/ maxHigh interval)
		  angStep (/ maxAngle (/ maxAngle 2))
		  x sx y sy z sz
		  a (list x y z)
	)
	(repeat interval
		(progn 
			(STD-SLEEP 2)
			(setq z (+ z zStep)
				  x (+ x xStep)
				  y (+ y yStep)
				  b (list x y z)
			)
			(command "move" ent "" a b)
			(setq a b)
			; rotate here
			(command "ucs" ent "")
			(command "ucs" "y" "90")
			(command "rotate" ent "" a angStep)
			(command "ucs" "w")
		)
	)
	(command "ucs" ent "")
	(command "ucs" "y" "90")
	(command "rotate" ent "" epos (- 0 maxAngle))
	(command "ucs" "w")
	(command "move" ent "" epos (get_position end))
	(setq name (get_name_by_ent player ent))
	(set_pos_by_name player name end)
)

(defun move_piece (player ent start end / name)
	(command "move" ent "" (get_position start) (get_position end))
	(setq name (get_name_by_ent player ent))
	(set_pos_by_name player name end)
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
(defun c:rc (/ player j name ipos pos ent)
	(initialize)
	(foreach player '(0 1) ;0 is white, 1 is black
		(setq j 0)
		(repeat (/ TOTAL_PIECES 2)
			; add all pieces
			(if (= player 0)
				(setq name (nth 0 (nth j white_Ipos))
					  ipos (nth 1 (nth j white_Ipos))
					  pos (nth 1 (nth j white_pos))
					  ent (get_ent_by_name player name)
				)
				(setq name (nth 0 (nth j black_Ipos))
					  ipos (nth 1 (nth j black_Ipos))
					  pos (nth 1 (nth j black_pos))
					  ent (get_ent_by_name player name)
				)
			)
			(move_piece player ent pos ipos)
			; next
			(setq j (+ j 1))
		)
	)
	(set_view)
	(princ)
)

; start playing game
(defun c:stg (/ cmdType st end ent name flag turns cmd mode)
	(initialize)
	(setvar "cmdecho" 0)
	(setq cmd (if (setq cmd (getstring "\nSelect game mode <p2p>: ")) cmd "p2p"))
	(if (cmd = p2p) (setq mode 0) (setq mode 1)) ;mode0: p2p, mode1: load from file

	; open file
	(if (= mode 1) (setq fp (open "game1.txt" "r")))

	(setq flag 1)
	(setq turns 0) ;0 is white, 1 is black
	(while (= flag 1)
		(if (= mode 1) (STD-SLEEP 30)) ;delay if input from file
		(if (= turns 0)
			(progn
				(if (= mode 0) 
					(setq cmd (getstring "\nPlayer white : "))
					(setq cmd (read-line fp))
				)
			)
			(progn
				(if (= mode 0) 
					(setq cmd (getstring "\nPlayer black : "))
					(setq cmd (read-line fp))
				)
			)
		);end if
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
				;moving
				(setq st   (substr cmd 1 2)
					  end  (substr cmd 4 2)
					  name (get_name_by_pos turns st)
					  ent  (get_ent_by_name turns name)
				)
				(move_piece_a turns ent st end)
				
				(if (= cmdType "x") ;kill if needed
					(progn
						(setq st   (substr cmd 4 2)
							  name (get_name_by_pos turns st)
							  ent  (get_ent_by_name turns name)
						)
						(move_piece_k turns ent st "GRAVE")
					)
				)
			)
		);end cond
		(if (= turns 0) (setq turns 1) (setq turns 0))
	);end while
	(princ)
)

(defun c:show (/)
	(princ "\n ====== DEBUG =====\n")
	(princ "\n white_pos: \n")
	(princ white_pos)
	(princ "\n white_ents: \n")
	(princ white_ents)
	(princ "\n black_pos: \n")
	(princ black_pos)
	(princ "\n black_ents: \n")
	(princ black_ents)
	(princ)
)