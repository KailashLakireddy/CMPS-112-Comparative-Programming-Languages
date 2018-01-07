#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

;;Create the lable-table
(define *label-table* (make-hash))
;;Create the function table
(define *function-table* (make-hash))
;;Create the variable table
(define *variable-table* (make-hash))








;;Get and Set functions for the function-table, lable-table, variable table
(define (function-get key)
        (hash-ref *function-table* key '(no such key in
                                         function-table)))
(define (function-put! key value)
        (hash-set! *function-table* key value))

(define (lable-get key)
        (hash-ref *label-table* key '()))
		
(define (lable-put! key value)
        (hash-set! *label-table* key value ))

(define (variable-get key)
        (hash-ref *variable-table* key '(no such key in 
                                          variable-table)))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))
		
		


		
;;func? checks if the function is in the function-table 
(define (func? function)
	(if (list? function)
	(if (procedure? (function-get (car function))) #t #f)
	#f)
)




;; var checks if the variale is in the variable-table
(define (var? variable)
	(if (number? (variable-get variable )) #t #f)
)		
		
	

(define (is-size1? expr)
(if(equal? (length (cdr expr)) 1) #t #f)
)


(define (is-size3? expr)
(if(equal? (length (cdr expr)) 3) #t #f)
)
	
	
	
(define (isinput? expr)
 (if (equal? (car expr) 'input) #t #f)
)


;; Code provided by the Teacher

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)



(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)



(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)



(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
				  program))))
				  
				  



;(define (input-sbir y )
 
;;(map double (list 1 2 3 4 5))
 ;(map (lambda (x) (let-sbir x (read)) (size (variable-get x))) y )  
;)
	
(define (printHash table) 	
 (hash-for-each table
        (lambda (key value)
                (printf "~s :  = ~s~n" key  value)
    )))
	
	
	
	
(define (sbir-input . expr)
  (variable-put! 'inputcount 0)
  (define (get-input expr)
     (when (not (null? (car expr)))
        (variable-put! (car expr) (void))
        (let ((object (read)))
      (cond [(eof-object? object)(variable-put! 'inputcount -1)]
       [(number? object)(let-sbir (car expr) object)
        (variable-put! 'inputcount (+ (variable-get 'inputcount) 1))]
          [else (begin (printf "invalid number: ~a~n" object)
                                )] )) 
         (when (not (null? (cdr expr)))
     (get-input (cdr expr)))
   )
  )
  (get-input expr)
)

	
(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(
        (inputcount    0)
		(e             2.718281828459045235360287471352662497757247093)
        (pi            3.141592653589793238462643383279502884197169399)
     ))		
		




(for-each
    (lambda (item)
            (function-put! (car item) (cadr item)))
    `(
        (log10   ,(lambda (x) (/ (log x) (log 10.0)))) 	
        (log10_2 0.301029995663981195213738894724493026768189881)
		(log, (lambda(x)(log (if (equal? x 0) 0.0 x))))
        (sqrt_2  1.414213562373095048801688724209698078569671875)
		(sqrt, sqrt)
		(+ ,+)
		(- ,-) 
		(* ,*)
        (/       ,(lambda (x y)  (divide-sbir x y)))
		(%       ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y)))) 
		(abs ,abs) 
		(<       , <)
        (>       , >)
        (=       , (lambda (x y) (eqv? x y)))
        (<=      , (lambda (x y) (<= x y)))
        (>=      , (lambda (x y) (>= x y)))
        (<>      ,(lambda (x y) (not (equal? x y))))
        (^       ,(lambda (x y) (expt x y)))
		(floor, floor)
        (ceil, ceiling) 
		(round, round)
		(exp, exp)
		(sin, sin) 
		(cos, cos) 
		(tan, tan)
        (asin, asin)
		(acos, acos) 
        (atan,    (lambda(x)(atan (if (equal? x 0) 0.0 x))))
        (print,   (lambda args (print-sbir args)))
		(dim,     (lambda (x) (variable-put! (car x) (make-vector (cadr x)))))
        (let,     (lambda (x y) (let-sbir x y)))
		(input,   (lambda (x) (let-sbir x (read)) (size (variable-get x))))
        (goto,    (lambda (x) (lable-get x)))
     ))





	 
	 
	 
	 
	 
;;Printer function
(define (print-sbir input)
 
    (for-each (lambda (item)
	(if (pair? item)
		(cond 
		    ( (equal?(length input )1)
		(display (vector-ref (variable-get (caar input)) (-(cadar input)1))))
          ( (equal? (length input)4)  (display (vector-ref (variable-get (car(cadddr input))) (-(cadr(cadddr input))1)))
		))
	(display item))) input)
    (newline)
)


;;Divide-sbir that handles division by 0)
(define (divide-sbir x y)
	(if (= y 0)
		(if (= x 0) "nan.0"
			(if (> x 0)
				"+inf.0"
				"-inf.0"))
	(/ x y))
)




(define (let-sbir variable value)
	(if (var? value) (let-sbir variable (variable-get value))
	(if (list? variable)
			(if (var? (cadr variable)) 
				(vector-set! (variable-get (car variable)) (-(variable-get (cadr variable))1) value)
		(vector-set! (variable-get (car variable)) (-(cadr variable)1) value))
	(variable-put! variable value))
))



(define (size input)
	(cond ((< input 0) (let-sbir 'inputcount (- 1)))
		  ((= input 0) (let-sbir 'inputcount 0))
		  (else 
		  	(define (size-help x len)
		  		(if (>= x 1)
		  			(size-help (/ x 10) (+ len 1)) 
		  			(let-sbir 'inputcount len)))
		  			(size-help input 0)		  	
		  )
	)
)


;;If statement used in loops
(define (if. x y lines)
	(if (evalexpr x) (lable-get y) (cdr lines))
)




(define (start filename program)
    (setsetlabel-help program) 	;;Sets setlabel-helps
	(interpret program) ;;Interprets program
)


;; Tail recursion applied to the setlabel-help table
(define (setsetlabel-help program)
	(define (setlabel-help lines)
		(cond  ((null? lines) lines) 									;;if the list is `()
		       ((null? (cdar lines))(setlabel-help (cdr lines)))         ;;if first line is only (#)
		       ((list? (cadar lines)) (setlabel-help (cdr lines)))
		       (else (lable-put!(cadar lines) lines)		             ;;default (# labl (stmt))
				(setlabel-help (cdr lines)))
		)	
	)
   (setlabel-help program)
 )

 
 

 
 ;;Evaluates the statements
(define (evalexpr expr)
	(cond ((null? expr) expr)
              ((number? expr) expr)
              ((string? expr) expr)
	    	  ((var? expr) (variable-get expr))
	    	  ((func? expr) 
	    	  	(cond
	    	  		     ((equal? (car expr) 'let) (let-sbir (cadr expr) (evalexpr (caddr expr))))
					     ((isinput? expr) 
                          
						    (apply sbir-input  (cdr expr)))
								
	    	  		     (else (apply(function-get (car expr)) (map evalexpr (cdr expr))))   
			    )
			  )
              ((pair? expr) (map evalexpr expr))
              (else expr)
    )

)				  


;;((equal? (car expr) 'input)  ( if  (=(length (cdr (expr))) 1)   (apply(function-get (car expr))(cdr expr))  (apply(function-get (car expr))(cdr expr))   ))


;;Tail Recursion, interprets program line by line
	(define (interpret program)
		(define (inter lines)
			(cond  ((null? lines) lines) 						;;if the list is `()
			       ((null? (cdar lines)) (inter (cdr lines)))	;;if first line is only (#)
			       ((list? (cadar lines)) 						;;if first line is (# (stmt))
					(cond ((equal? 'goto (caadar lines)) (inter (evalexpr (cadar lines))))
					      ((equal? 'if (caadar lines)) (inter (if. (car (cdadar lines)) (cadr (cdadar lines)) lines)))
						  ;;((equal? 'input  b c (cadar lines)) (print (cadar lines))  (evalexpr  (cadar lines)))
						  (else (evalexpr (cadar lines)) 
								(inter (cdr lines)))
					))
			       ((null? (cddar lines)) (inter (cdr lines)))	;;if line is (# labl)
			       (else 
			       (cond  ((equal? 'goto (car (caddar lines))) (inter (evalexpr (caddar lines)))) ;;Run inter on setlabel-help
					      ((equal? 'if (car (caddar lines))) 
					      		(inter (if. (cadr (caddar lines)) (caddr (caddar lines)) lines))) ;;Run inter on setlabel-help
						  (else (evalexpr (caddar lines))
								(inter (cdr lines)))
						)
					)
			)	
		)
		(inter program)
	)
	
	
 

				  
;;Main function (given)
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (start sbprogfile program))))

(main (vector->list (current-command-line-arguments)))