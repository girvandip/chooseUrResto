(deftemplate restoran
	(slot nama (type STRING))
	(slot isSmoker )
	(slot minBudget (type INTEGER))
	(slot maxBudget (type INTEGER))
	(multislot dresscode (type STRING))
	(slot hasWifi)
	(slot latitude (type FLOAT))
	(slot longitude (type FLOAT))
)


;;;----------;;;
;;; FUNCTION ;;;
;;;----------;;;

(deffunction bool-to-int (?bool) 
	(if (eq ?bool TRUE) then 1 else 0)
)

(deffunction hitungJarak (?x1 ?x2 ?y1 ?y2)
	(+ (* (- ?x1 ?x2) (- ?x1 ?x2)) (* (- ?y1 ?y2) (- ?y1 ?y2)))
)

;;;---------;;;
;;;--RULE---;;;
;;;---------;;;

(defrule determine-namaUser
	(not (found ?))	
	=>
	(printout t "Masukkan nama anda : ")
	(bind ?nama (read))
	(assert (nama ?nama))	
)

(defrule determine-isSmoker 
	(not (found ?))
	=>
	(printout t "Apakah anda seorang perokok? (yes/no) ")
	(bind ?u-isSmoker (readline))
	(if (eq ?u-isSmoker "no") then (assert (isSmoker FALSE)) 
	 else (assert (isSmoker TRUE)) 
	 )
)

(defrule determine-minBudget
	(not (found ?))	
	=>
	(printout t "Berapakah budget minimum anda? (0-9999) ")
	(bind ?u-minBudget (read))
	(if (integerp ?u-minBudget)
	then (assert (minBudget ?u-minBudget)) 
	)
	(while (not (integerp ?u-minBudget)) do
	(printout t "Berapakah budget minimum anda? (0-9999) ")
	(bind ?u-minBudget (read))
	(if (integerp ?u-minBudget)
	then (assert (minBudget ?u-minBudget))))	
)

(defrule determine-maxBudget
	(not (found ?))	
	=>
	(printout t "Berapakah budget maksimum anda? (0-9999) ")
	(bind ?u-maxBudget (read))
	(if (integerp ?u-maxBudget)
	then (assert (maxBudget ?u-maxBudget)) 
	)
	(while (not (integerp ?u-maxBudget)) do
	(printout t "Berapakah budget maksimum anda? (0-9999) ")
	(bind ?u-maxBudget (read))
	(if (integerp ?u-maxBudget)
	then (assert (maxBudget ?u-maxBudget))))	
)

(defrule determine-dresscode
	(not (found ?))
	=>
	(printout t "Baju apa yang Anda pakai? (casual/formal/informal) ")
	(bind ?u-dresscode (readline))
	(if (or(eq ?u-dresscode "informal") (eq ?u-dresscode "formal")) then (assert (dresscode ?u-dresscode)) 
	else (assert (dresscode "casual"))
	)
)

(defrule determine-hasWifi
	(not (found ?))
	=>
	(printout t "Apakah anda ingin restoran yang memiliki wifi? (yes/no) ")
	(bind ?u-hasWifi (readline))
	(if (eq ?u-hasWifi "no")  then (assert (hasWifi FALSE)) 
	 else (assert (hasWifi TRUE)) 
	 )
)	

(defrule determine-latitude
	(not (found ?))	
	=>
	(printout t "Berapa latitude anda? ")
	(bind ?latitude (read))
	(if (floatp ?latitude)
	then (assert (latitude ?latitude)) 
	)
	(while (not (floatp ?latitude)) do
	(printout t "Berapa latitude anda? ")
	(bind ?latitude (read))
	(if (floatp ?latitude)
	then (assert (latitude ?latitude))))	
)

(defrule determine-longitude
	(not (found ?))	
	=>
	(printout t "Berapa longitude anda? ")
	(bind ?longitude (read))
	(if (floatp ?longitude)
	then (assert (longitude ?longitude)) 
	)
	(while (not (floatp ?longitude)) do
	(printout t "Berapa longitude anda? ")
	(bind ?longitude (read))
	(if (floatp ?longitude)
	then (assert (longitude ?longitude))))	
)

(defrule checkdresscode
	(restoran 
		(nama ?rnama)
		(dresscode $?rdresscode)
		)
	(dresscode ?u-dresscode)
	(test (member ?u-dresscode ?rdresscode))
	=>
	(assert (hasildresscode ?rnama 1))
)

(defrule check
	?a <- (restoran 
			(nama ?rnama) 
			(isSmoker ?risSmoker) 
			(minBudget ?rminBudget)
			(maxBudget ?rmaxBudget)
			(dresscode $?rdresscode)
			(hasWifi ?rhasWifi)
			(latitude ?rlatitude)
			(longitude ?rlongitude)
			)
	?c <- (isSmoker ?u-isSmoker)
	?d <- (minBudget ?u-minBudget)
	?e <- (maxBudget ?u-maxBudget)
	;?f <- (dresscode ?u-dresscode)
	?g <- (hasWifi ?u-hasWifi)
	?h <- (latitude ?u-latitude)
	?i <- (longitude ?u-longitude)
	;(test (member ?u-dresscode ?rdresscode))
	=>
	(assert (hasil ?rnama
		(+
			(bool-to-int (eq ?risSmoker ?u-isSmoker))
			(bool-to-int (or (and (<= ?rmaxBudget ?u-maxBudget) (<= ?rminBudget ?u-minBudget))(and (<= ?rmaxBudget ?u-maxBudget) (>= ?rminBudget ?u-minBudget))))
			(bool-to-int (eq ?rhasWifi ?u-hasWifi))
		))
	)
)

(defrule totalhasil
	(hasildresscode ?rnama-dresscode ?value-dresscode)
	?f <- (hasil ?rnama-single ?value-single)
	(test (eq ?rnama-dresscode ?rnama-single))
	=>
	(retract ?f)
	(assert (total ?rnama-single (+ ?value-dresscode ?value-single)))
)

(defrule deleteHasil
	?f <- (hasil ?rnama ?value)
	=>
	(retract ?f)
	(assert (total ?rnama ?value))
)

(defrule assertJarak
	(restoran (nama ?rnama) (latitude ?lat1) (longitude ?long1))
	(latitude ?lat2)
	(longitude ?long2)
	=>
	(assert (jarak ?rnama (hitungJarak ?lat1 ?lat2 ?long1 ?long2)))
)

(defrule klasifikasi
	(total ?rnama ?value)
	=>
	(if (eq ?value 4) 
		then (assert (kelas ?rnama "very recommendable"))
		else (if (or (eq ?value 2) (eq ?value 3))
			then (assert (kelas ?rnama "recommendable")) 
			else (assert (kelas ?rnama "not recommendable")))	
	)
)

(defrule printresult1
	(declare (salience -10))
	(kelas ?rnama ?value)
	(test (eq ?value "very recommendable"))
	=>
	(format t "%-10s: %-15s%n" ?rnama ?value)
)

(defrule printresult2
	(declare (salience -100))
	(kelas ?rnama ?value)
	(test (eq ?value "recommendable"))
	=>
	(format t "%-10s: %-15s%n" ?rnama ?value)
)

(defrule printresult3
	(declare (salience -1000))
	(kelas ?rnama ?value)
	(test (eq ?value "not recommendable"))
	=>
	(format t "%-10s: %-15s%n" ?rnama ?value)
)

;;;---------;;;
;;;--Fact---;;;
;;;---------;;;

(deffacts listRestoran
	(restoran 
			  (nama "restoran A")
			  (isSmoker TRUE)
			  (minBudget 1000)
			  (maxBudget 2000)
			  (dresscode "casual")
			  (hasWifi TRUE)
			  (latitude -6.8922186)
			  (longitude 107.5886173))
	(restoran 
			  (nama "restoran B")
			  (isSmoker TRUE)
			  (minBudget 1200)
			  (maxBudget 2500)
			  (dresscode "informal")
			  (hasWifi TRUE)
			  (latitude -6.224085)
			  (longitude 106.785981))
	(restoran 
			  (nama "restoran C")
			  (isSmoker TRUE)
			  (minBudget 2000)
			  (maxBudget 4000)
			  (dresscode "formal")
			  (hasWifi FALSE)
			  (latitude -5.2145285)
			  (longitude 106.864259))
	(restoran 
			  (nama "restoran D")
			  (isSmoker FALSE)
			  (minBudget 500)
			  (maxBudget 1400)
			  (dresscode "formal")
			  (hasWifi FALSE)
			  (latitude -6.9005363)
			  (longitude 107.6222191))
	(restoran 
			  (nama "restoran E")
			  (isSmoker TRUE)
			  (minBudget 1000)
			  (maxBudget 2000)
			  (dresscode "informal" "casual")
			  (hasWifi TRUE)
			  (latitude -6.2055617)
			  (longitude 106.8001597))
	(restoran 
			  (nama "restoran F")
			  (isSmoker FALSE)
			  (minBudget 2500)
			  (maxBudget 5000)
			  (dresscode "informal")
			  (hasWifi TRUE)
			  (latitude -6.9045679)
			  (longitude 107.6399745))
	(restoran 
			  (nama "restoran G")
			  (isSmoker TRUE)
			  (minBudget 1300)
			  (maxBudget 3000)
			  (dresscode "casual")
			  (hasWifi TRUE)
			  (latitude -6.1881082)
			  (longitude 106.7844409))
	(restoran 
			  (nama "restoran H")
			  (isSmoker FALSE)
			  (minBudget 400)
			  (maxBudget 1000)
			  (dresscode "informal")
			  (hasWifi FALSE)
			  (latitude -6.9525133)
			  (longitude 107.605290))
	(restoran 
			  (nama "restoran I")
			  (isSmoker FALSE)
			  (minBudget 750)
			  (maxBudget 2200)
			  (dresscode "informal" "casual")
			  (hasWifi TRUE)
			  (latitude -6.9586985)
			  (longitude 107.7092281))
	(restoran 
			  (nama "restoran J")
			  (isSmoker FALSE)
			  (minBudget 1500)
			  (maxBudget 2000)
			  (dresscode "casual")
			  (hasWifi TRUE)
			  (latitude -6.2769732)
			  (longitude 106.775133))
 )