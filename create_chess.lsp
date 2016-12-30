(defun initialize (/)
	(graphscr)(command "osnap" "none")
	(command "erase" "all" "")
	(command "ucs" "w")
	(command "vpoint" "1,-1,1")
	(command "layer" "m" "black" "color" "blue" "" "" "" "" "")
	(command "layer" "m" "white" "color" "red" "" "" "" "" "")
	(command "layer" "m" "board" "color" "yellow" "" "" "" "" "")
	(command "layer" "s" "0" "")
)

;input code and return a position in autoCAD
(defun get_position (code /)
	(last (assoc code BOARD))
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
	(setq BOARD '(("a8" (-154 154 0)) ("b8" (-110 154 0)) ("c8" (-66 154 0)) ("d8" (-22 154 0)) ("e8" (22 154 0)) ("f8" (66 154 0)) ("g8" (110 154 0)) ("h8" (154 154 0)) ("a7" (-154 110 0)) ("b7" (-110 110 0)) ("c7" (-66 110 0)) ("d7" (-22 110 0)) ("e7" (22 110 0)) ("f7" (66 110 0)) ("g7" (110 110 0)) ("h7" (154 110 0)) ("a6" (-154 66 0)) ("b6" (-110 66 0)) ("c6" (-66 66 0)) ("d6" (-22 66 0)) ("e6" (22 66 0)) ("f6" (66 66 0)) ("g6" (110 66 0)) ("h6" (154 66 0)) ("a5" (-154 22 0)) ("b5" (-110 22 0)) ("c5" (-66 22 0)) ("d5" (-22 22 0)) ("e5" (22 22 0)) ("f5" (66 22 0)) ("g5" (110 22 0)) ("h5" (154 22 0)) ("a4" (-154 -22 0)) ("b4" (-110 -22 0)) ("c4" (-66 -22 0)) ("d4" (-22 -22 0)) ("e4" (22 -22 0)) ("f4" (66 -22 0)) ("g4" (110 -22 0)) ("h4" (154 -22 0)) ("a3" (-154 -66 0)) ("b3" (-110 -66 0)) ("c3" (-66 -66 0)) ("d3" (-22 -66 0)) ("e3" (22 -66 0)) ("f3" (66 -66 0)) ("g3" (110 -66 0)) ("h3" (154 -66 0)) ("a2" (-154 -110 0)) ("b2" (-110 -110 0)) ("c2" (-66 -110 0)) ("d2" (-22 -110 0)) ("e2" (22 -110 0)) ("f2" (66 -110 0)) ("g2" (110 -110 0)) ("h2" (154 -110 0)) ("a1" (-154 -154 0)) ("b1" (-110 -154 0)) ("c1" (-66 -154 0)) ("d1" (-22 -154 0)) ("e1" (22 -154 0)) ("f1" (66 -154 0)) ("g1" (110 -154 0)) ("h1" (154 -154 0)) ))
	(setq white_pos '(("k" "e1") ("q" "d1") ("br" "f1") ("bl" "c1") ("nr" "g1") ("nl" "b1") ("cr" "h1") ("cl" "a1") ("p1" "a2") ("p2" "b2") ("p3" "c2") ("p4" "d2") ("p5" "e2") ("p6" "f2") ("p7" "g2") ("p8" "h2") ))
	(setq black_pos '(("k" "e7") ("q" "d7") ("br" "f7") ("bl" "c7") ("nr" "g7") ("nl" "b7") ("cr" "h7") ("cl" "a7") ("p1" "a8") ("p2" "b8") ("p3" "c8") ("p4" "d8") ("p5" "e8") ("p6" "f8") ("p7" "g8") ("p8" "h8") ))
	(setq white_ents '(("k") ("q") ("br") ("bl") ("nr") ("nl") ("cr") ("cl") ("p1") ("p2") ("p3") ("p4") ("p5") ("p6") ("p7") ("p8") ))
	(setq black_ents '(("k") ("q") ("br") ("bl") ("nr") ("nl") ("cr") ("cl") ("p1") ("p2") ("p3") ("p4") ("p5") ("p6") ("p7") ("p8") ))
)

(defun mvlyr (ent lyrname / elist)
	(setq elist (entget ent))
	(entmod (subst (cons 8 lyrname) (assoc 8 elist) elist))
)

(defun set_view (/)
	(command "ucs" "w")
	(command "vscurrent" "s") ;solid
	(command "vpoint" "1,-1,1")
	(command "zoom" "E" "zoom" "0.8x")
	(command "grid" "off")
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
		((= player 0) (subst (cons name tmpEnt) (assoc name white_ents) white_ents))
		((= player 1) (subst (cons name tmpEnt) (assoc name black_ents) black_ents))
	)	
)

(defun C:ts (/)
	(initialize)
	(init_constant)

	; add board
	(setq boardPath (strcat SETDIR "/" (last (assoc "board" FILELIST)) ))
	(load_pieces "board" "board" boardPath SPBoard)

	(foreach player '(0 1) ;0 is white, 1 is black
		(setq j 0)
		(repeat (/ TOTAL_PIECES 2)
			; add all pieces
			(if (= player 0)
				(setq name (nth 0 (nth j white_pos))
					  pos (nth 1 (nth j white_pos))
				)
				(setq name (nth 0 (nth j black_pos))
					  pos (nth 1 (nth j black_pos))
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