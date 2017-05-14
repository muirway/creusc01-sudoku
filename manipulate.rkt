#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A set of all possible numbers to populate puzzle
(define intset (set 1 2 3 4 5 6 7 8 9) )


; A struct for a cell, containing the row, col and box indices as well as numbers (row by column)
(struct subgrid (row col box nums) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Gets the row position from position in flattened grip 
(define (which-row? position) (quotient position 9) )


; Gets the col position from position in flattened grip 
(define (which-col? position) (- position (* 9 (which-row? position) ) ) )


; Gets the box position from position in flattened grip 
(define (which-box? position) (+ ( quotient (which-col? position) 3) (* 3 (quotient (which-row? position) 3 ) ) ) )  


; A particular cell is solved if it only contains a singleton set
(define (solved-cell? cell) 
	(= 1 (set-count cell )
)

; Given row, col and box, determine if a particular cell is either on the same row or on the same column or in the same box, but not all together
(define (relevant-subgrid? sbgrd row col box) 
	(and 
		(or (equal? row (subgrid-row sbgrd) ) (equal? col (subgrid-col sbgrd) ) (equal? box (subgrid-box sbgrd) ) )  
		(not (and (equal? row (subgrid-row sbgrd) ) (equal? col (subgrid-col sbgrd) ) (equal? box (subgrid-box sbgrd) ) ) )
	)
)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Transforms puzzle from a numbers only puzzle to a grid that contains the numbers as a single value set if not 0 or as the intset if 0
(define (transform puzzle)
	(map (lambda (alist)
		(map (lambda number)
			(if (equal? number 0) 
				intset 
				(set number) 
			)
			alist 
		) )	
		puzzle 
	) 
)

; Does same as above but flattens puzzle
(define (transform-flat puzzle) 
	(map (lambda (number) 
		(if (equal? number 0) intset (set number) )
		(flatten puzzle) 
	) )
)


; Defines a custom map that applies a function (which takes itself a number and a position) iteratively over a list; function intended for use is populate-subgrid below
(define (custom-map func alist)
	(let iterator ( [il (range 0 (length alist) ) ]
					[inputlist alist]
					[outputlist () ] )
		(if (null? inputlist)-b
			(reverse outputlist)
			(iterator 	(cdr il)
						(cdr inputlist)
						(cons (func (car inputlist) (car il) ) outputlist ) ) 
		) 
	) 
) 


; If number in cell in puzzle is 0, substitute in set containing ints from 1-9, if net, "setify" the number in the cell
(define (populate-subgrid number position)
	(if (equal? number 0)
		(subgrid (which-row? position) (which-col? position) (which-box? position) intset)
		(subgrid (which-row? position) (which-col? position) (which-box? position) (set number) )
	)
)

; Run the populate-subgrid function with custom map over flattened puzzle structure
(define (transform-subgrid puzzle)
	(custom-map populate-subgrid (flatten puzzle) )
)

; Get just the numbers (either singleton or set) from a subgrid cell
(define (extract-subgrid-nums sbgrd) 
	(let (nums (subgrid-nums subgrd) ) )
	(if (equal? 1 (set-count nums) ) 
		(set-first nums)
		(nums) 
	)
)


; Extract all numbers (either singleton or set) from entire grid
(define (extract-row-nums rownum grid) 
	(let iterator ( 	[carsubgrid (car grid) ]
				[partialgrid (cdr grid) ]
				[accum '() ] )
		(let ( ( acclist (if (equal? rownum (subgrid-row carsubgrid) )
								(cons (extract-subgrid-nums carsubgrid) accum)	
								accum) ) )
			(if (null? partialgrid)	
				(reverse acclist)
				(iterator 	(car partialgrid)
							(cdr partialgrid)
							acclist) 
			) 
		)
	)
)


; Reformat the grid to a nurmal puzzle form 
(define (retransform-to-puzzle grid)
	(let iterator (	[puzzle (list)]
					[nrow 8] )
		(if (> 0 nrow)
			puzzle
			(iterator 	(cons (extract-row-nums nrow grid) puzzle)
					(- nrow 1) )
		)
	)
)


; This is what the assignment test puzzle looks like translated into Racket list of lists
(define (test-puzzle)
	(list
   		(list 0 2 5 0 0 1 0 0 0)
   		(list 1 0 4 2 5 0 0 0 0)
	   	(list 0 0 6 0 0 4 2 1 0)
   		(list 0 4 0 0 0 0 3 2 0)
	 	(list 6 0 0 0 2 0 0 0 9)
   		(list 0 8 7 0 0 0 0 6 0)
	   	(list 0 9 1 5 0 0 6 0 0)
   		(list 0 0 0 0 7 8 1 0 3)
	   	(list 0 0 0 6 0 0 5 9 0)
	)
)


; This is what the assignment test puzzle looks like as a grid (consisting of subgrid cell structs) for the purposes of the program  
(define (test-grid)
	(list
 		(subgrid 0 0 0 (set 1 2 3 4 5 6 7 8 9))
 		(subgrid 0 1 0 (set 2))
		(subgrid 0 2 0 (set 5))
		(subgrid 0 3 1 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 0 4 1 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 0 5 1 (set 1))
		(subgrid 0 6 2 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 0 7 2 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 0 8 2 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 1 0 0 (set 1))
		(subgrid 1 1 0 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 1 2 0 (set 4))
		(subgrid 1 3 1 (set 2))
		(subgrid 1 4 1 (set 5))
		(subgrid 1 5 1 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 1 6 2 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 1 7 2 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 1 8 2 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 2 0 0 (set 1 2 3 4 5 6 7 8 9))  
		(subgrid 2 1 0 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 2 2 0 (set 6))
		(subgrid 2 3 1 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 2 4 1 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 2 5 1 (set 4))
		(subgrid 2 6 2 (set 2))
		(subgrid 2 7 2 (set 1))
		(subgrid 2 8 2 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 3 0 3 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 3 1 3 (set 4))  
		(subgrid 3 2 3 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 3 3 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 3 4 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 3 5 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 3 6 5 (set 3))
		(subgrid 3 7 5 (set 2))
		(subgrid 3 8 5 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 4 0 3 (set 6))
		(subgrid 4 1 3 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 4 2 3 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 4 3 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 4 4 4 (set 2))
		(subgrid 4 5 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 4 6 5 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 4 7 5 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 4 8 5 (set 9))
		(subgrid 5 0 3 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 5 1 3 (set 8))
		(subgrid 5 2 3 (set 7))
		(subgrid 5 3 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 5 4 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 5 5 4 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 5 6 5 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 5 7 5 (set 6))
		(subgrid 5 8 5 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 6 0 6 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 6 1 6 (set 9))
		(subgrid 6 2 6 (set 1))
		(subgrid 6 3 7 (set 5))
		(subgrid 6 4 7 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 6 5 7 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 6 6 8 (set 6))
		(subgrid 6 7 8 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 6 8 8 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 7 0 6 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 7 1 6 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 7 2 6 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 7 3 7 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 7 4 7 (set 7))
		(subgrid 7 5 7 (set 8))
		(subgrid 7 6 8 (set 1))
		(subgrid 7 7 8 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 7 8 8 (set 3))
		(subgrid 8 0 6 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 8 1 6 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 8 2 6 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 8 3 7 (set 6))
		(subgrid 8 4 7 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 8 5 7 (set 1 2 3 4 5 6 7 8 9))
		(subgrid 8 6 8 (set 5))
		(subgrid 8 7 8 (set 9))
		(subgrid 8 8 8 (set 1 2 3 4 5 6 7 8 9))
	)
)
















