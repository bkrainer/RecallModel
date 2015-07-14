(defvar *response* nil)
(defvar *answer* nil)
(defvar *means* nil)
(defvar *correct* nil)

(defun read-letter (letter delay)

	(new-word-sound letter delay))

(defun do-trial (string)

	(reset)

	(let* ((window (open-exp-window "Serial Recall w/ Recoding"
				  											   :visible nil
				  												 :width 300
				  												 :height 300)))

		(install-device window)

		(dotimes  (i (length string))
			(read-letter (nth i string)(* i 0.8333))

		(setf *response* nil)
		(run 30 :real-time nil))))

(defun average (list) (/ (reduce #'+ list) (length list)))

(defun	strip(string)
	
	(mapcan #'(lambda (x) (and (not (or (string-equal "" x) (string-equal "DONE" x))) (list x))) string)

)

(defun number-correct()
	(let ((correct 0))
		(dotimes (i (length *answer*) correct)
			(when (string-equal (subseq  (concat-string *answer*) i (+ i 1)) (subseq (concat-string (reverse *response*)) i (+ i 1)))
				(incf correct)))))

(defun concat-string (list)
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(defun join (strings tojoin)
	(dolist (s strings)
		(setf tojoin (concatenate 'string tojoin s))))

(defmethod device-speak-string ((win rpm-window) text)
  (push text *response*))

(defun print-results()
	(print (float (average *correct*))))

(defun do-experiment ()
	(setf *correct* nil)
	(let ((strings (permute-list '(("B" "M" "I" "" "D" "K" "I" "" "O" "L" "L" "" "H" "D" "P" "" "DONE")
									 ("D" "F" "I" "" "Y" "I" "F" "" "I" "Y" "D" "" "B" "A" "M" "" "DONE")
									 ("B" "F" "I" "" "I" "A" "C" "" "Y" "T" "N" "" "S" "J" "W" "" "DONE")
									 ("S" "A" "U" "" "A" "E" "U" "" "D" "R" "F" "" "F" "K" "J" "" "DONE")
									 ("B" "J" "L" "" "O" "S" "D" "" "P" "U" "C" "" "A" "M" "R" "" "DONE")
									 ("O" "M" "R" "" "N" "U" "G" "" "X" "E" "E" "" "P" "G" "J" "" "DONE")
									 ("C" "E" "E" "" "I" "T" "C" "" "C" "S" "S" "" "M" "E" "B" "" "DONE")
									 ("N" "G" "P" "" "B" "C" "A" "" "B" "C" "B" "" "M" "U" "C" "" "DONE")
									 ("D" "F" "P" "" "M" "D" "A" "" "E" "C" "N" "" "E" "O" "C" "" "DONE")
									 ("F" "O" "C" "" "S" "A" "N" "" "S" "B" "F" "" "B" "M" "S" "" "DONE")
									 ("O" "C" "T" "" "T" "M" "H" "" "M" "O" "G" "" "S" "D" "L" "" "DONE")
									 ("S" "G" "M" "" "D" "S" "L" "" "I" "T" "M" "" "F" "I" "T" "" "DONE")
									 ("A" "D" "C" "" "M" "G" "O" "" "P" "L" "I" "" "M" "A" "D" "" "DONE")
									 ("H" "F" "C" "" "P" "G" "R" "" "W" "I" "W" "" "U" "R" "E" "" "DONE")
									 ("O" "W" "W" "" "A" "X" "L" "" "H" "C" "T" "" "T" "C" "H" "" "DONE")
									 ("C" "T" "A" "" "A" "T" "S" "" "C" "A" "A" "" "H" "R" "L" "" "DONE")
									 ("F" "L" "N" "" "L" "B" "M" "" "L" "S" "M" "" "B" "A" "N" "" "DONE")
									 ("H" "L" "N" "" "N" "M" "M" "" "F" "L" "A" "" "I" "S" "V" "" "DONE")
									 ("T" "A" "E" "" "B" "D" "N" "" "O" "A" "D" "" "I" "A" "M" "" "DONE")
									 ("B" "D" "T" "" "M" "A" "F" "" "L" "A" "T" "" "D" "K" "S" "" "DONE"))))



				(acronyms (permute-list '(("I" "B" "M" "" "I" "D" "K" "" "L" "O" "L" "" "P" "H" "D" "" "DONE")
									  ("I" "D" "F" "" "F" "Y" "I" "" "D" "I" "Y" "" "M" "B" "A" "" "DONE")
									  ("F" "B" "I" "" "C" "I" "A" "" "N" "Y" "T" "" "W" "S" "J" "" "DONE")
									  ("U" "S" "A" "" "U" "A" "E" "" "F" "D" "R" "" "J" "F" "K" "" "DONE")
									  ("L" "B" "J" "" "D" "O" "S" "" "C" "P" "U" "" "R" "A" "M" "" "DONE")
									  ("R" "O" "M" "" "G" "N" "U" "" "E" "X" "E" "" "J" "P" "G" "" "DONE")
									  ("E" "C" "E" "" "C" "I" "T" "" "S" "C" "S" "" "B" "M" "E" "" "DONE")
									  ("P" "N" "G" "" "A" "B" "C" "" "B" "B" "C" "" "C" "M" "U" "" "DONE")
									  ("P" "D" "F" "" "A" "M" "D" "" "N" "E" "C" "" "C" "E" "O" "" "DONE")
									  ("C" "F" "O" "" "N" "S" "A" "" "F" "S" "B" "" "S" "B" "M" "" "DONE")
									  ("T" "O" "C" "" "H" "T" "M" "" "G" "M" "O" "" "L" "S" "D" "" "DONE")
									  ("M" "S" "G" "" "L" "D" "S" "" "M" "I" "T" "" "T" "F" "I" "" "DONE")
									  ("C" "A" "D" "" "O" "M" "G" "" "I" "P" "L" "" "D" "M" "A" "" "DONE")
									  ("C" "H" "F" "" "R" "P" "G" "" "W" "W" "I" "" "E" "U" "R" "" "DONE")
									  ("W" "O" "W" "" "L" "A" "X" "" "T" "H" "C" "" "H" "T" "C" "" "DONE")
									  ("A" "C" "T" "" "S" "A" "T" "" "A" "C" "A" "" "L" "H" "R" "" "DONE")
									  ("N" "F" "L" "" "M" "L" "B" "" "M" "L" "S" "" "N" "B" "A" "" "DONE")
									  ("N" "H" "L" "" "M" "N" "M" "" "A" "F" "L" "" "V" "I" "S" "" "DONE")
									  ("E" "T" "A" "" "N" "B" "D" "" "D" "O" "A" "" "M" "I" "A" "" "DONE")
									  ("T" "B" "D" "" "F" "M" "A" "" "T" "L" "A" "" "S" "D" "K" "" "DONE")))))


		(dolist (s strings)
			(setf *answer* (strip s))
	  	(do-trial s)
			(push (number-correct) *correct*))

		(print "Average Letters Recalled -Nonwords:")
		(print-results)

    (setf *correct* nil)
		(dolist (a acronyms)
			(setf *answer* (strip a))
			(do-trial a)
			(push (number-correct) *correct*))

		(print "Average Letters Recalled - Acronyms:")
		(print-results)))

(clear-all)

(define-model recoding

	(sgp :v nil :esc t :lf 0.0 :rt -0.6 :bll 0.5 :ans 0.4 :trace-detail low)

  (chunk-type letter-sequence state let1 let2 let3 w pos chunk-pos)
  (chunk-type remember-pos i letter)
  (chunk-type position cur next)
  (chunk-type chunk-position cur-chunk next-chunk)
  (chunk-type acronym w l1 l2 l3)
  (chunk-type recoded acronym i)
  (chunk-type new-chunk let1 let2 let3 i)

	(add-dm
  (start isa chunk)
  (listening isa chunk)
  (encode isa chunk)
  (recall isa chunk)
  (done isa chunk)
  (recode isa chunk)
  (chunk-pos isa chunk)
  (speak-chunk isa chunk)
  (continue isa chunk)
  (first isa chunk)
  (second isa chunk)
  (third isa chunk)
  (fourth isa chunk)
  (fifth isa chunk)
  (sixth isa chunk)
  (seventh isa chunk)
  (eighth isa chunk)
  (ninth isa chunk)
  (tenth isa chunk)
  (eleventh isa chunk)
  (twelvth isa chunk)
  (end isa chunk)
  (a isa position cur first next second)
  (b isa position cur second next third)
  (c isa position cur third next fourth)
  (d isa position cur fourth next fifth)
  (e isa position cur fifth next sixth)
  (f isa position cur sixth next seventh)
  (g isa position cur seventh next eighth)
  (h isa position cur eighth next ninth)
  (i isa position cur ninth next tenth)
  (j isa position cur tenth next eleventh)
  (k isa position cur eleventh next twelvth)
  (l isa position cur twelvth next end)


  (chunk-inc1 isa chunk-position cur-chunk first next-chunk fourth)
  (chunk-inc2 isa chunk-position cur-chunk fourth next-chunk seventh)
  (chunk-inc3 isa chunk-position cur-chunk seventh next-chunk tenth)
  (chunk-inc4 isa chunk-position cur-chunk tenth next-chunk done)

  (ibm isa acronym w "IBM" l1 "I" l2 "B" l3 "M")
  (lol isa acronym w "LOL" l1 "L" l2 "O" l3 "L")
  (idk isa acronym w "IDK" l1 "I" l2 "D" l3 "K")
  (phd isa acronym w "PHD" l1 "P" l2 "H" l3 "D")

  (idf isa acronym w "IDF" l1 "I" l2 "D" l3 "F")
  (fyi isa acronym w "FYI" l1 "F" l2 "Y" l3 "I")
  (diy isa acronym w "DIY" l1 "D" l2 "I" l3 "Y")
  (mba isa acronym w "MBA" l1 "M" l2 "B" l3 "A")

  (fbi isa acronym w "FBI" l1 "F" l2 "B" l3 "I")
  (cia isa acronym w "CIA" l1 "C" l2 "I" l3 "A")
  (nyt isa acronym w "NYT" l1 "N" l2 "Y" l3 "T")
  (wsj isa acronym w "WSJ" l1 "W" l2 "S" l3 "J")

  (usa isa acronym w "USA" l1 "U" l2 "S" l3 "A")
  (uae isa acronym w "UAE" l1 "U" l2 "A" l3 "E")
  (fdr isa acronym w "FDR" l1 "F" l2 "D" l3 "R")
  (jfk isa acronym w "JFK" l1 "J" l2 "F" l3 "K")

  (lbj isa acronym w "LBJ" l1 "L" l2 "B" l3 "J")
  (dos isa acronym w "DOS" l1 "D" l2 "O" l3 "S")
  (cpu isa acronym w "CPU" l1 "C" l2 "P" l3 "U")
  (ram isa acronym w "RAM" l1 "R" l2 "A" l3 "M")

  (rom isa acronym w "ROM" l1 "R" l2 "O" l3 "M")
  (gnu isa acronym w "GNU" l1 "G" l2 "N" l3 "U")
  (exe isa acronym w "EXE" l1 "E" l2 "X" l3 "E")
  (jpg isa acronym w "JPG" l1 "J" l2 "P" l3 "G")

  (ece isa acronym w "ECE" l1 "E" l2 "C" l3 "E")
  (cit isa acronym w "CIT" l1 "C" l2 "I" l3 "T")
  (scs isa acronym w "SCS" l1 "S" l2 "C" l3 "S")
  (bme isa acronym w "BME" l1 "B" l2 "M" l3 "E")

  (png isa acronym w "PNG" l1 "P" l2 "N" l3 "G")
  (cmu isa acronym w "CMU" l1 "C" l2 "M" l3 "U")
  (abc isa acronym w "ABC" l1 "A" l2 "B" l3 "C")
  (bbc isa acronym w "BBC" l1 "B" l2 "B" l3 "C")

  (pdf isa acronym w "PDF" l1 "P" l2 "D" l3 "F")
  (amd isa acronym w "AMD" l1 "A" l2 "M" l3 "D")
  (nec isa acronym w "NEC" l1 "N" l2 "E" l3 "C")
  (ceo isa acronym w "CEO" l1 "C" l2 "E" l3 "O")

  (cfo isa acronym w "CFO" l1 "C" l2 "F" l3 "O")
  (nsa isa acronym w "NSA" l1 "N" l2 "S" l3 "A")
  (fsb isa acronym w "FSB" l1 "F" l2 "S" l3 "B")
  (sbm isa acronym w "SBM" l1 "S" l2 "B" l3 "M")

  (toc isa acronym w "TOC" l1 "T" l2 "O" l3 "C")
  (htm isa acronym w "HTM" l1 "H" l2 "T" l3 "M")
  (gmo isa acronym w "GMO" l1 "G" l2 "M" l3 "O")
  (lsd isa acronym w "LSD" l1 "L" l2 "S" l3 "D")

  (msg isa acronym w "MSG" l1 "M" l2 "S" l3 "G")
  (lds isa acronym w "LDS" l1 "L" l2 "D" l3 "S")
  (mit isa acronym w "MIT" l1 "M" l2 "I" l3 "T")
  (tfi isa acronym w "TFI" l1 "T" l2 "F" l3 "I")

  (cad isa acronym w "CAD" l1 "C" l2 "A" l3 "D")
  (omg isa acronym w "OMG" l1 "O" l2 "M" l3 "G")
  (dma isa acronym w "DMA" l1 "D" l2 "M" l3 "A")
  (ipl isa acronym w "IPL" l1 "I" l2 "P" l3 "L")

  (chf isa acronym w "CHF" l1 "C" l2 "H" l3 "F")
  (rpg isa acronym w "RPG" l1 "R" l2 "P" l3 "G")
  (wwi isa acronym w "WWI" l1 "W" l2 "W" l3 "I")
  (eur isa acronym w "EUR" l1 "E" l2 "U" l3 "R")

  (wow isa acronym w "WOW" l1 "W" l2 "O" l3 "W")
  (thc isa acronym w "THC" l1 "T" l2 "H" l3 "C")
  (htc isa acronym w "HTC" l1 "H" l2 "T" l3 "C")
  (lax isa acronym w "LAX" l1 "L" l2 "A" l3 "X")

  (act isa acronym w "ACT" l1 "A" l2 "C" l3 "T")
  (sat isa acronym w "SAT" l1 "S" l2 "A" l3 "T")
  (aca isa acronym w "ACA" l1 "A" l2 "C" l3 "A")
  (lhr isa acronym w "LHR" l1 "L" l2 "H" l3 "R")

  (nfl isa acronym w "NFL" l1 "N" l2 "F" l3 "L")
  (mlb isa acronym w "MLB" l1 "M" l2 "L" l3 "B")
  (mls isa acronym w "MLS" l1 "M" l2 "L" l3 "S")
  (nba isa acronym w "NBA" l1 "N" l2 "B" l3 "A")

  (nhl isa acronym w "NHL" l1 "N" l2 "H" l3 "L")
  (mnm isa acronym w "MNM" l1 "M" l2 "N" l3 "M")
  (afl isa acronym w "AFL" l1 "A" l2 "F" l3 "L")
  (vis isa acronym w "VIS" l1 "V" l2 "I" l3 "S")

  (eta isa acronym w "ETA" l1 "E" l2 "T" l3 "A")
  (nbd isa acronym w "NBD" l1 "N" l2 "B" l3 "D")
  (doa isa acronym w "DOA" l1 "D" l2 "O" l3 "A")
  (mia isa acronym w "MIA" l1 "M" l2 "I" l3 "A")

  (tbd isa acronym w "TBD" l1 "T" l2 "B" l3 "D")
  (fma isa acronym w "FMA" l1 "F" l2 "M" l3 "A")
  (tla isa acronym w "TLA" l1 "T" l2 "L" l3 "A")
  (sdk isa acronym w "SDK" l1 "S" l2 "D" l3 "K")


  (goal isa letter-sequence state listening let1 nil let2 nil let3 nil w nil pos first chunk-pos first))

	(P detected-sound

		=goal>
			state listening
			pos =pos

		=aural-location>

		?aural>
			state free

		?imaginal>
			state free

		?retrieval>
			state free

		==>

		=goal>
			state encode

		+retrieval>
			isa position
			cur =pos

		+imaginal>

		+aural>
			event =aural-location)

	(P encode-letter1

		=goal>
			state encode
			let1 nil

		=aural>
		 - content ""
		   content =content

		=imaginal>

		=retrieval>
			cur =pos1
			next =pos2

		==>

		=imaginal>
			isa remember-pos
			i =pos1
			letter =content
		-imaginal>

		+retrieval>
			cur =pos2

		=goal>
			state listening
			let1 =content
			pos =pos2)

	(P encode-letter2

		=goal>
			state encode
		- let1 nil
		  let2 nil
		  pos =pos

		=aural>
		 - content ""
		   content =content

		=imaginal>

		=retrieval>
			cur =pos1
			next =pos2


		==>

		=imaginal>
			i =pos1
			letter =content
		-imaginal>

		+retrieval>
			cur =pos2

		=goal>
			state listening
			let2 =content
			pos =pos2)

	(P encode-letter3

		=goal>
			state encode
		 - let1 nil
		 - let2 nil
		   let3 nil
		   pos =pos
		
		=aural>
		 - content ""
		   content =content

		=retrieval>
			cur =pos1
			next =pos2

		=imaginal>

		==>

		=imaginal>
			i =pos1
			letter =content
		-imaginal>

		=goal>
			state listening
			let3 =content
			pos =pos2)

	(P encode-chunk

		=goal>
			state encode
			let1 =l1
			let2 =l2
			let3 =l3

		=aural>
			content ""

		?imaginal>
			state free

		==>

		=goal>
			state recode

		+imaginal>

		+retrieval> 
			l1 =l1
			l2 =l2
			l3 =l3)

	(P continue-listening

		=goal>
			state recode
			let1 =l1
			let2 =l2
			let3 =l3
			pos =p
			chunk-pos =cp

		?retrieval>
			state error

		=imaginal>

		==>

		=goal>
			state listening
			let1 nil
			let2 nil
			let3 nil
			chunk-pos =p

		+aural-location>
			:attended nil)

	(P recode-chunk

		=goal> 
			state recode
			pos =pos
			chunk-pos =chunk-pos

		=retrieval>
			isa acronym
			l1 =l1
			l2 =l2
			l3 =l3
		
		==>

		=retrieval>
			isa recoded
			acronym =retrieval
			i =chunk-pos
		-retrieval>

		=goal>
			state listening
			let1 nil
			let2 nil
			let3 nil
			chunk-pos =pos

		+aural-location>
			:attended nil)


	(P end-listening

		=goal>
			state encode

		=aural>
			content "DONE"

		?imaginal>
			state free

		==>

		+retrieval>
			i first

		+imaginal>

		=goal>
			state recall
			pos first)

	(p leave-blank
		
		=goal>
			state recall
			pos =p

		?retrieval>
			state error

		?vocal>
			state free

		==>

		=goal>
			state continue

		+vocal>
			cmd speak
			string "_"

		+retrieval>
			cur =p)

	(P continue
		=goal>
			state continue
			pos =p

		=retrieval>
			cur =pos1
			next =pos2

		==>

		=goal>
			state recall
			pos =pos2

		+retrieval>
			i =pos2)

	(p recall-chunk
		=goal>
			state recall
			pos =p

		=retrieval>
			i =pos
			acronym =a
			w =w

		==>

		+retrieval>
			isa chunk-position
			cur-chunk =p

		=goal>
			state speak
			pos =p
			w =w)

	(p recall-letters
		=goal>
			state recall
		- pos end 
			pos =p

		=retrieval>
			i =pos
			letter =letter

		==>

		+retrieval>
			cur =p

		=goal>
			state speak
			pos =p
			let1 =letter)

	(p speak-letter
		=goal>
			state speak
			let1 =letter

		=retrieval>
			cur =pos1
			next =pos2

		?vocal>
			state free

		==>

		=goal>
			state recall
			pos =pos2

		+vocal>
			cmd speak
			string =letter

		+retrieval>
			isa remember-pos
			i =pos2)

	(p speak-chunk

		=goal>
			state speak
		- w nil
			w =w

		=retrieval>
			cur-chunk =pos1
			next-chunk =pos2

		?vocal>
			state free

		==>

			=goal>
				state recall
				pos =pos2

			+vocal>
				cmd speak
				string =w

			+retrieval>
				i =pos2)

	(p finish

		=goal>
			state recall
			pos end

		==>

		=goal>
			state done)

	(set-base-levels (a 1000000))
	(set-base-levels (b 1000000))
	(set-base-levels (c 1000000))
	(set-base-levels (d 1000000))
	(set-base-levels (e 1000000))
	(set-base-levels (f 1000000))
	(set-base-levels (g 1000000))
	(set-base-levels (h 1000000))
	(set-base-levels (i 1000000))
	(set-base-levels (j 1000000))
	(set-base-levels (k 1000000))
	(set-base-levels (l 1000000))

	(set-base-levels (chunk-inc1 1000000))
	(set-base-levels (chunk-inc2 1000000))
	(set-base-levels (chunk-inc3 1000000))
	(set-base-levels (chunk-inc4 1000000))

	(set-base-levels (ibm 1000000))
	(set-base-levels (lol 1000000))
	(set-base-levels (idk 1000000))
	(set-base-levels (phd 1000000))

	(set-base-levels (idf 1000000))
	(set-base-levels (fyi 1000000))
	(set-base-levels (diy 1000000))
	(set-base-levels (mba 1000000))

	(set-base-levels (fbi 1000000))
	(set-base-levels (cia 1000000))
	(set-base-levels (nyt 1000000))
	(set-base-levels (wsj 1000000))

	(set-base-levels (usa 1000000))
	(set-base-levels (uae 1000000))
	(set-base-levels (fdr 1000000))
	(set-base-levels (jfk 1000000))

	(set-base-levels (lbj 1000000))
	(set-base-levels (dos 1000000))
	(set-base-levels (cpu 1000000))
	(set-base-levels (ram 1000000))

	(set-base-levels (rom 1000000))
	(set-base-levels (gnu 1000000))
	(set-base-levels (exe 1000000))
	(set-base-levels (jpg 1000000))

	(set-base-levels (ece 1000000))
	(set-base-levels (cit 1000000))
	(set-base-levels (scs 1000000))
	(set-base-levels (bme 1000000))

	(set-base-levels (png 1000000))
	(set-base-levels (abc 1000000))
	(set-base-levels (bbc 1000000))
	(set-base-levels (cmu 1000000))

	(set-base-levels (pdf 1000000))
	(set-base-levels (amd 1000000))
	(set-base-levels (nec 1000000))
	(set-base-levels (ceo 1000000))

	(set-base-levels (cfo 1000000))
	(set-base-levels (sbm 1000000))
	(set-base-levels (fsb 1000000))
	(set-base-levels (nsa 1000000))

	(set-base-levels (toc 1000000))
	(set-base-levels (htm 1000000))
	(set-base-levels (lsd 1000000))
	(set-base-levels (gmo 1000000))

	(set-base-levels (msg 1000000))
	(set-base-levels (lds 1000000))
	(set-base-levels (mit 1000000))
	(set-base-levels (tfi 1000000))

	(set-base-levels (cad 1000000))
	(set-base-levels (omg 1000000))
	(set-base-levels (dma 1000000))
	(set-base-levels (ipl 1000000))

	(set-base-levels (chf 1000000))
	(set-base-levels (rpg 1000000))
	(set-base-levels (wwi 1000000))
	(set-base-levels (eur 1000000))

	(set-base-levels (wow 1000000))
	(set-base-levels (thc 1000000))
	(set-base-levels (htc 1000000))
	(set-base-levels (lax 1000000))

	(set-base-levels (act 1000000))
	(set-base-levels (sat 1000000))
	(set-base-levels (aca 1000000))
	(set-base-levels (lhr 1000000))

	(set-base-levels (nfl 1000000))
	(set-base-levels (mlb 1000000))
	(set-base-levels (mls 1000000))
	(set-base-levels (nba 1000000))

	(set-base-levels (nhl 1000000))
	(set-base-levels (mnm 1000000))
	(set-base-levels (afl 1000000))
	(set-base-levels (vis 1000000))

	(set-base-levels (eta 1000000))
	(set-base-levels (nbd 1000000))
	(set-base-levels (doa 1000000))
	(set-base-levels (mia 1000000))

	(set-base-levels (tbd 1000000))
	(set-base-levels (fma 1000000))
	(set-base-levels (tla 1000000))
	(set-base-levels (sdk 1000000))

	(goal-focus goal)

)
