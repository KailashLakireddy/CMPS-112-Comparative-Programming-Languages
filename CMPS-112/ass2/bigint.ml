



open Printf
open List

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])
	
	
	

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                        ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

     
	 
	let delete_zeros list1 =
        let rec delete_zeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = delete_zeros' cdr
                 in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'
        in delete_zeros' list1
	
			
			
    let rec cmp list1 list2 = match (list1, list2) with
        | [], []                 ->  0
        | list1, []              ->  1
        | [], list2              -> -1
        | x::xs, y::ys           -> 
            let v = cmp xs ys
            in if v = 0 && x != y
               then (if x > y
                    then 1
                    else (if x < y
                    then -1
                    else 0))
              else v
		
			
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)
		  
		  
	let rec sub' num1 num2 carry = match (num1, num2, carry) with
    | list1, [], 0 -> list1
    | list1, [], carry -> delete_zeros(sub' list1 [- carry] 0)
    | [], _::_, _ -> failwith "Invalid_argument sub'" 
    | h1::t1, h2::t2, carry ->
      let diff = h1 - h2 + carry + radix
      in diff mod radix :: delete_zeros (sub' t1 t2 (diff / radix - 1))

					   
			   
    let double number = add' number number 0
	
	let add_ val1 val2 = add' val1 val2 0
	
	let sub_ val1 val2 = sub' val1 val2 0

    let rec mul' (multiplier, po2, multiplicand') = 
        match cmp po2 multiplier with 
        | 1 -> multiplier, [0]
        | _ ->  let remainder, product = 
                mul' (multiplier, double po2, double multiplicand')
                in  (match  cmp po2 remainder with
                | 1 -> remainder, product
                | _ ->  sub_ remainder po2, 
                                       add_ product multiplicand' )
    
 
    let add (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        match  (neg1 = neg2) with
        | true ->   Bigint (neg1, add_ val1 val2)
        | false ->  match cmp val1 val2  with
            | 1   ->  Bigint (neg1, sub_ val1 val2)
            | -1  ->  Bigint (neg2, sub_ val2 val1)
            | _   ->  zero

			
			
    let sub (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
      match  (neg1 = neg2)  with
	   | false -> Bigint (neg1, add_ val1 val2)
       | true ->  match cmp val1 val2  with
            | 1  ->  Bigint (neg1, (sub_ val1 val2))
            | -1 ->  let sign = if neg1 = Pos then Neg else Pos
                     in  Bigint (sign, (sub_ val2 val1) )
            | _  ->  zero
         
   
    let mul_egy (multiplier, multiplicand) = 
        let _, product = mul' (multiplier, [1], multiplicand)
        in product
  
   
    let mul (Bigint (neg1, multiplier)) (Bigint (neg2, multiplicand)) =
         match  (neg1 = neg2) with 
        | true ->  (match cmp multiplier multiplicand with 
             | -1 -> Bigint (Pos, mul_egy (multiplicand, multiplier))
			 | _ -> Bigint (Pos, mul_egy (multiplier, multiplicand)))
        | false ->   ( match cmp multiplier multiplicand with 
             | -1 -> Bigint (Neg, mul_egy (multiplicand, multiplier))
			 | _  -> Bigint (Neg, mul_egy (multiplier, multiplicand)))

   
    let rec divrem' (dividend, po2, divisor') =
        match  cmp divisor' dividend with 
        | 1  ->  [0], dividend
        | _  ->  let quotient, remainder = 
                divrem' (dividend, double po2, double divisor')
                in  (match cmp divisor' remainder with 
           | 1 ->  quotient, remainder
           |_  -> add_ quotient po2, sub_ remainder divisor')

 
    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

	
    let div_egy (dividend, divisor) = 
        let quotient, _ = divrem (dividend, divisor)
      in quotient
 
	
    let div (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
       match  (neg1 = neg2) with 
       | true  ->   Bigint (Pos, div_egy (dividend, divisor))
       | false ->  Bigint (Neg, div_egy (dividend, divisor))

		
		
    let rem_egy(dividend, divisor) =
       let  _, remainder = divrem (dividend, divisor)
     in remainder

	 
    let rem (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
       match  (neg1 = neg2) with 
       | true  ->   Bigint (Pos, rem_egy (dividend, divisor))
       | false ->  Bigint (Neg, rem_egy (dividend, divisor)) 
   
    
    
    let even number = 
       match cmp (rem_egy(number, [2])) [] with  
       | 0 ->  true
       | _ ->  false  
	   
	   
    
   
    let rec pow'(base, expt, result) = 
       match  cmp expt [0]  with
       | 0 ->  result      
       | _ ->  (match  even expt with 
                | true -> pow'(mul_egy (base, base), 
				           div_egy (expt, [2]), result) 
                | false-> pow'(base, sub' expt [1] 0, 
				           mul_egy (base, result))  )

	   
	   
   
    let pow (Bigint (neg1, base)) (Bigint (neg2, expt)) =
        
        if neg2 = Neg
        then zero  
        else if neg1 = Pos
             then Bigint (Pos, pow'(base, expt, [1]))
        else if even expt
             then Bigint (Pos, pow'(base, expt, [1]))
        else Bigint (Neg, pow'(base, expt, [1]))
				
		
end

