(ns test.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  
  (declare term?, factor?, expr?)
  
  ;***** split a string by whitespace as a vector of characters *****
  (defn split [x]
  	(str/split x #" ")
  )
  
  ;**** returns true if it's a low priority operator such as + or - ******
  (defn lowOP? [input]
  	(not (clojure.string/blank? (re-matches #"[+-]" input)))
  )
  
  ;**** returns true if it's an operator +-/*, used instead of checking if its lowOP or highOP ****
  (defn OP? [input]
  	(not (clojure.string/blank? (re-matches #"[+*-\/]" input)))
  )
  
  ;**** returns true if it's a high priority operator such as * or /
  (defn highOP? [input]
  	(not (clojure.string/blank? (re-matches #"[*\/]" input)))
  )
  
  ;**** returns true if input is alphanumeric *****
  (defn id? [input]
  	(not (clojure.string/blank? (re-matches #"[A-Za-z0-9]" input)))
  )
  
  ;**** returns true if input is a left parenthesis *****
  (defn lparen? [input]
  	(= input "(")
  )
  
  ;**** returns true if input is a right parenthesis *****
  (defn rparen? [input]
  	(= input ")")
  )
 
 (comment ...
  ** returns true if parenthesis inside a string is balance, false if not
  ** code is taken from "https://gist.github.com/damionjunk/1396939"
  ** I don't understand it that much so can't comment on it but it works!
 ...)

  (defn balance? [input]
    (zero? (count (reduce (fn [v ch]
                            (if (= ch \()
                              (conj v ch)
                              (if (= ch \))
                                (if (= \( (peek v))
                                  (pop v)
                                  (conj v ch))
                                v)))
                           []
                           input)))
  )
  
  (comment ...
  ** pre- a vector of characters that doesn't fit in term or expression, mainly parenthesis in this case
  ** post- returns true if there's no invalid order of input without balancing parenthesis
  **    Look at current token and look ahead 1 token to see if they're ordered correctly
  **    If the order is correct, send the rest to expr? or term? based on current token
  ** 	returns true when the vector is empty or the last vector item is an id or right parenthesis
  ...)
  (defn factor? [input]
  	(if (first input)
		(cond
			(= (count input) 1)  ;input size == 1
				(if (or (id? (first input)) (rparen? (first input)))
					true
					(do false (print "Operator expected >> "))
				)
			(> (count input) 1)  ;inpu size > 1
					(cond
						(lparen? (first input))   ;if first token is left parenthesis
							(expr? (rest input))  ;send the rest after parentehsis to expr?
						
						;if a right parenthesis is follow by a + or - operator
						(and (rparen? (first input)) (lowOP? (second input)))
							(expr? (rest (rest input)))  ;check if an expression follows it
							
						;if a right parenthesis is followed by a * or / operator
						(and (rparen? (first input)) (highOP? (second input)))
							(term? (rest (rest input)))	;check if a term follows it
							
						;if it's a right parenthesis followed by another right parenthesis
						(and (rparen? (first input)) (rparen? (second input)))
							;remove the first parenthesis and pass input to factor
							(factor? (rest input))    
						
						:default (do false (print "Missing operator >> "))
					)
		)
		true
	)
  )
  
  (comment ...
  ** pre - vector of tokens
  ** post - return true if only 1 item in vector and it's an ID
  **		else work with 2 tokens at a time. sending the rest to term or factor if it doesn't
  **		fit as term
  **		Return false if last item in vector is not an ID
  ...)
  (defn term? [input]
  	(if (first input)
		(cond
			(= (count input) 1)
				(if	(id? (first input))
					true
					(do false (print "ID expected >> "))
				)
			(> (count input) 1)
				(cond
					(and (id? (first input)) (highOP? (second input))) ;if id follow by * or / "a * b..."
						(term? (rest (rest input)))        ;then call term on "b...."
						
					(and (id? (first input)) (lowOP? (second input)))  ;if id follow by + or - "a + b..."
						(expr? input)                   ;then call expr on "a + b....."
						
					(and (id? (first input)) (rparen? (second input))) ;if id follow by )  "b )"
						(factor? (rest input))   ;send to factor for evaluation
						
					(and (id? (first input)) (lparen? (second input))) ;if id follow by (  "a ("
						 (do false	(print "Expected operator before ("))	;error
						 
					(or (rparen? (first input)) (OP? (first input)))  ;first input is ) or operator
						;error, first input should always be id or (
						(do false (print "Misplaced ) or operator"))	
						
					:default (factor? input)		
				)
		)
		(do false (print "Expecting ID >> "))
	)
  )
  
  (comment ...
  ** pre - vector of characters
  ** post - return true if there's only 1 item and it's an ID
  **		if more than 1 item, look 2 tokens at a time to see if the order of tokens match
  ** 		if order doesn't match, send to term
  **		if end of vector is reach and it's empty, return false
  ...)
  (defn expr? [input]
  	(if (first input)
		(cond
			(= (count input) 1) 
				(if (id? (first input))
					true
					false
				)
			
			(> (count input) 1)
				(cond
				 	(and (id? (first input)) (lowOP? (second input)))  ;id followed by + or -
						(expr? (rest (rest input)))				;start with the next 2 tokens
					(or (OP? (first input)) (rparen? (first input)))   ;if first char is operator or )
						(do false (print "Double operators. Expect ID instead >> "))
					:default (term? input)
				)
		)
		(do false (print "Expression empty or ended incorrectly >> "))
	)
  )
  
  (comment ...
  ** pre - a vector of characters
  ** post - print ACCEPT if expression is valid, and REJECT otherwise
  ...)
  (defn evaluateExpression [expression]
  	(if (expr? expression)
		(println "ACCEPT")
		(println "REJECT")
	)
  )
  
  (comment ...
  ** pre - a file name as string
  ** post - print ACCEPT and REJECT for each line in the file
  **	Get each line from the file
  **	Check if the parenthesises are balanced
  **	If balance, continue to evaluate the expression
  **	else print REJECT
  ...)
  (defn readFile [fileName]	  
	  (with-open [rdr (io/reader fileName)]
	  	(doseq [line (line-seq rdr)]
			(if (balance? line)
				(evaluateExpression (split line))
				(println "Mismatch parenthesis >> REJECT")
			)
		)
	  )
  )	  
  
  ;main
  (readFile "input.txt")
)
