#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Need this for the program to run
(require "manipulate.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A struct to hold a subgrid snapshot, containing before after and whether its number has changed (by reduction)
(struct subgrid-snap (before subgrid after has-changed?) #:transparent)

; Same as above, but for the whole "gridyfied" puzzle
(struct grid-snap (grid has-changed?) #:transparent)

; Stuct that holds singular numbers across row col and box
(struct singular-nums-struct (row-nums col-nums box-nums) #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Iteration through grid applying func to each subgrid, returns grid and whether it changed.
(define (grid-iterate func grid)
	(let iterate (	[tried '()]
        			[to-try grid]
             		[grd-has-changed? #f])
    	(if (null? to-try)
        	(grid-snap tried grd-has-changed?)
        	(let ((sg 	(func tried 
            	       	(car to-try) 
                	    (cdr to-try))))
	        	(iterate 	(append (subgrid-snap-before sg) 
    	        	       		(list (subgrid-snap-subgrid sg)))
        	        		(subgrid-snap-after sg)
            	    		(or (subgrid-snap-has-changed? sg) grd-has-changed?)
				)
			)
		)
	)
)

; Iteration across grid and applying func to each subgrid, returns concatenated list of all f returns.
(define (grid-accum func init grid)
	(let iterate ([manip-ed '()]
             	[to-manip grid]
             	[accum init])
		(if (null? to-try)
        	accum
        	(let* 
				((manip (car to-try)) (sg (func manip accum)))
          		(iterate 	(append manip-ed (list manip))
                			(cdr to-manip)
                			sg)
			)
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Removes a number from a list of numbers (if orginal cell entry was 0), of any subgrid that is relevant (defined in manipulate.rkt) to the row col box.
(define (remove row col box num)
	(lambda (before sg-old after)
    	(if (relevant-subgrid? sg-old row col box)
        	(let 
				((sg-new (subgrid 
                	        (subgrid-row sg-old)
                    	    (subgrid-col sg-old)
                        	(subgrid-box sg-old)
                        	(set-remove (subgrid-nums sg-old) num))))
	             (subgrid-snap before
    	                   sg-new
        	               after
            	           (not (equal? (subgrid-nums sg-old) (subgrid-nums sg-new))))
			)
	      	(subgrid-snap before sg-old after #f)
		)	
	)
)

; Extract row, col, box information from a subgrid, iterating through whole grid and applying remove.
(define (remove-iterate before sg after)
	(if (equal? 1 (set-count (subgrid-nums sg)))
    	(let ((rm-lbd (remove 	(subgrid-row sg)
                                (subgrid-col sg)
                                (subgrid-box sg)
                                (set-first (subgrid-nums sg)))))
        	(let ((grd-before (grid-iterate rm-lbd before))
            		(grd-after (grid-iterate rm-lbd after)))
          		(subgrid-snap 	(grid-snap-grid grd-before)
                         		sg
                         		(grid-snap-grid grd-after)
                         		(or (grid-snap-has-changed? g-before) (grid-snap-has-changed? g-after))
				)
			)
		)
      	(subgrid-snap before sg after #f)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests if a subgrid is in a particular row, column or box, then removes numbers from a singular-nums-stuct
(define (singular-nums row col box)
	(lambda (subgrid unq-nums)
    	(if (relevant-subgrid? subgrid row col box)
        	(singular-nums-struct 
         		(set-subtract (singular-nums-struct-row-nums unq-nums) 
             		(if (equal? (subgrid-row subgrid) row) (subgrid-nums subgrid) (set))
				)
         		(set-subtract (singular-nums-struct-col-nums unq-nums) 
                	(if (equal? (subgrid-col subgrid) col) (subgrid-nums subgrid) (set))
				)
         		(set-subtract (singular-nums-struct-box-nums unq-nums) 
                	(if (equal? (subgrid-box subgrid) box) (subgrid-nums subgrid) (set))
				)
			)
        	unq-nums
		)
	)
)

; Interogate grid to build struct of singular numbers across a particular row, col and box of a particular subgrid; change number of cell to the number if only one number is returned
(define (singular-nums-iterate before sg after)
	(if (<= 1 (set-count (subgrid-nums sg)))
    	(let* 
			(
				(singulars 
					(grid-accum (singular-nums 	(subgrid-row sg)
                                           		(subgrid-col sg)
                                           		(subgrid-box sg)
								)
                                (singular-nums-struct 	(subgrid-nums sg)
                                                  		(subgrid-nums sg)
	                                                  	(subgrid-nums sg)
								)
                                (append before after)
					)
				)
             	(singulars-flat 	(set-union 
										(singular-nums-struct-row-nums singulars) 
                                     	(singular-nums-struct-col-nums singulars) 
                                     	(singular-nums-struct-box-nums singulars)
									)
				)
			)
           	(if (equal? (set-count singulars-flat) 1)
            	(subgrid-snap 	before 
                            	(subgrid 	(subgrid-row sg)
                                  			(sungrid-col sg)
                                  			(subgrid-box sg)
                                  			singulars-flat)
                            	after
                            	#t
				)
             	(subgrid-snap before sg after #f))
		)
      	(subgrid-snap before sg after #f)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Zooms in on a particular subgrid and then applies remove iteratively if there is only one number, or singular-nums otherwise.
(define (zoom before sg after)
	(if (equal? 1 (set-count (subgrid-nums sg)))
  		(remove-iterate before sg after)
  		(singular-nums-iterate before sg after)
	)
)

; Consume grid, iterate focus function through it, apply recursion if board has changed, return the board otherwise.
(define (solve-grid grid)
	(let ((new-grid (grid-iterate zoom grid)))
    	(if (grid-snap-has-changed? new-grid)
        	(solve-grid (grid-snap-grid new-grid))
        	(grid-snap-grid new-grid)
		)
	)
)

; Solves the puzzle by transforming it to a grid, using solve-grid and re-transforming back to puzzle format
(define (solve puzzle)
	(retransform-to-puzzle 
   	(solve-grid (transform-subgrid puzzle)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide subgrid-snap grid-snap grid-iterate grid-accum remove remove-iterate singular-nums singular-nums-iterate singular-nums-struct singular-nums-struct-row-nums singular-nums-struct-col-nums singular-nums-struct-box-nums zoom solve-grid solve)
