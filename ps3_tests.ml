
open Ps3 ;;
  
(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)
let _ = assert(negate {neg = false; coeffs = []}
                    = {neg = false; coeffs = []})
let _ = assert(negate {neg = true; coeffs = [1; 2]}
                    = {neg = false; coeffs = [1; 2]})


	        
(* Some advice on automated testing:

   Here is an automated testing function that checks every pair of
   integers between count and max to verify that the bignum
   representations of count and max are the same if and only if count
   and max are.

   Use this function to help you catch potential edge cases. While
   this kind of automated testing is helpful, it is still important
   for you to think about what cases may be difficult for your
   algorithm. Also, think about what inputs to run the testing
   function on. If you're having trouble isolating a bug, you can try
   printing out which values cause an assert failure.

   You may find that adding this sort of testing function for other
   functions is useful.  *)
	      
let rec test_equal (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(equal (fromInt count) (fromInt max) = (count = max)) in
    test_equal (count + 1) max ;;

(* Examples of using the automated testing function *)

let () = test_equal (-10000) 10000 ;;
let () = test_equal 10000 (-10000) ;;
let () = test_equal (-10000) 9999 ;;
