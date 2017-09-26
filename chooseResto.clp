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

(defrule check
	?a <- (restoran 
			(nama ?rnama) 
			(isSmoker ?risSmoker) 
			(minBudget ?rminBudget)
			(maxBudget ?rmaxBudget)
			(dresscode ?rdresscode)
			(hasWifi ?rhasWifi)
			(latitude ?rlatitude)
			(longitude ?rlongitude)
			)
	?b <- (nama "USER")
	?c <- (isSmoker ?u-isSmoker)
	?d <- (minBudget ?u-minBudget)
	?e <- (maxBudget ?u-maxBudget)
	?f <- (dresscode ?u-dresscode)
	?g <- (hasWifi ?u-hasWifi)
	;?h <- (latitude ?u-latitude)
	;?i <- (longitude ?u-longitude)

	=>
	(assert (hasil ?rnama
		(+
			(bool-to-int (eq ?risSmoker ?u-isSmoker))
			(bool-to-int (or (and (<= ?rmaxBudget ?u-maxBudget) (<= ?rminBudget ?u-minBudget))(and (<= ?rmaxBudget ?u-maxBudget) (>= ?rminBudget ?u-minBudget))))
			;(bool-to-int (>  (member$ ?rdresscode ?u-dresscode) 0)
			(bool-to-int (eq ?rhasWifi ?u-hasWifi))
		))
	)
)


(defrule printresult
	(declare (salience 10000))
	(hasil ?rnama ?a)
	=>
	(printout t ?rnama ?a)
)




;;;---------;;;
;;; Fact	;;;
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
	
 )