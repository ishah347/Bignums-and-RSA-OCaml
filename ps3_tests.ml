
open Ps3 ;;
  
(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)
let _ = assert(negate {neg = false; coeffs = []}
                    = {neg = false; coeffs = []});;
let _ = assert(negate {neg = true; coeffs = [1;2]}
                    = {neg = false; coeffs = [1;2]});;
let _ = assert(negate {neg = false; coeffs = [1;3]}
                    = {neg = true; coeffs = [1;3]});;


	        
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
    let _ = assert(equal (from_int count) (from_int max) = (count = max)) in
    test_equal (count + 1) max ;;

(* Examples of using the automated testing function *)

let () = test_equal (-10000) 10000 ;;
let () = test_equal (-10000) 9999 ;;
let () = test_equal (-10000) (-1000) ;;
let () = test_equal (-9999) (-1000) ;;
let () = test_equal 1000 10000 ;;
let () = test_equal 1000 9999 ;;
let () = test_equal 10000 (-10000) ;;
let () = test_equal (-10000) (-10000) ;;
let () = test_equal 10000 10000 ;;

let rec test_less (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(less (from_int count) (from_int max) = (count < max)) in
    test_less (count + 1) max ;;

let () = test_less (-10000) 10000 ;;
let () = test_less (-10000) 9999 ;;
let () = test_less (-10000) (-1000) ;;
let () = test_less (-9999) (-1000) ;;
let () = test_less 1000 10000 ;;
let () = test_less 1000 9999 ;;
let () = test_less 10000 (-10000) ;;
let () = test_less (-10000) (-10000) ;;
let () = test_less 10000 10000 ;;

let rec test_greater (count : int) (min : int) : unit =
  if count < min then ()
  else
    let _ = assert(greater (from_int count) (from_int min) = (count > min)) in
    test_greater (count - 1) min ;;

let () = test_greater 10000 (-10000);;
let () = test_greater 9999 (-10000);;
let () = test_greater (-1000) (-10000);;
let () = test_greater (-1000) (-9999);;
let () = test_greater 10000 1000;;
let () = test_greater 9999 1000;;
let () = test_greater (-10000) 10000;;
let () = test_greater (-10000) (-10000) ;;
let () = test_greater 10000 10000 ;;

let _ = assert(from_int 0 = {neg = false; coeffs = []});;
let _ = assert(from_int 100 = {neg = false; coeffs = [100]});;
let _ = assert(from_int 10000 = {neg = false; coeffs = [10;0]});;
let _ = assert(from_int max_int = 
                        {neg = false; coeffs = [4;611;686;18;427;387;903]});;
let _ = assert(from_int (-100) = {neg = true; coeffs = [100]});;
let _ = assert(from_int (-10000) = {neg = true; coeffs = [10;0]});;
let _ = assert(from_int min_int = 
                        {neg = true; coeffs = [4;611;686;18;427;387;904]});;

let _ = assert(to_int {neg = false; coeffs = []} = Some 0);;
let _ = assert(to_int {neg = false; coeffs = [100]} = Some 100);;
let _ = assert(to_int {neg = false; coeffs = [10;0]} = Some 10000);;
let _ = assert(to_int {neg = false; coeffs = [4;611;686;18;427;387;903]} =
                      Some max_int);;
let _ = assert(to_int {neg = false; coeffs = [4;611;686;18;427;387;904]} =
                      None);;
let _ = assert(to_int {neg = true; coeffs = [100]} = Some (-100));;
let _ = assert(to_int {neg = true; coeffs = [10;0]} = Some (-10000));;
let _ = assert(to_int {neg = true; coeffs = [4;611;686;18;427;387;904]} =
                      Some min_int);;
let _ = assert(to_int {neg = true; coeffs = [4;611;686;18;427;387;905]} =
                      None);;

let _ = assert(plus (from_int 0) (from_int 0) = from_int 0);;
let _ = assert(plus (from_int 0) (from_int 1) = from_int 1);;
let _ = assert(plus (from_int (-1)) (from_int 0) = from_int (-1));;
let _ = assert(plus (from_int 10000) (from_int (-10000)) = from_int 0);;
let _ = assert(plus (from_int (-10000)) (from_int 10000) = from_int 0);;
let _ = assert(plus (from_int 10000) (from_int 10000) = from_int 20000);;
let _ = assert(plus (from_int 10000) (from_int (-5000)) = from_int 5000);;
let _ = assert(plus (from_int (-5000)) (from_int 10000) = from_int 5000);;
let _ = assert(plus (from_int (-10000)) (from_int 5000) = from_int (-5000));;
let _ = assert(plus (from_int 5000) (from_int (-10000)) = from_int (-5000));;
let _ = assert(plus (from_int (-10000)) (from_int (-10000)) = from_int (-20000));;
let _ = assert(plus (from_string "123456789") (from_string "987654321") =
                     {neg = false; coeffs = [1; 111; 111; 110]});;
let _ = assert(plus (from_string "~123456789") (from_string "987654321") =
                     {neg = false; coeffs = [864; 197; 532]});;
let _ = assert(plus (from_string "123456789") (from_string "~987654321") =
                     {neg = true; coeffs = [864; 197; 532]});;
let _ = assert(plus (from_string "~123456789") (from_string "~987654321") =
                     {neg = true; coeffs = [1; 111; 111; 110]});;     

let _ = assert(times (from_string "123456789") (from_string "987654321") =
                     {neg = false; coeffs = [450; 410; 713; 198; 660; 269]});;
let _ = assert(times (from_string "~123456789") (from_string "987654321") =
                     {neg = true; coeffs = [450; 410; 713; 198; 660; 269]});;
let _ = assert(times (from_string "123456789") (from_string "~987654321") =
                     {neg = true; coeffs = [450; 410; 713; 198; 660; 269]});;
let _ = assert(times (from_string "~123456789") (from_string "~987654321") =
                     {neg = false; coeffs = [450; 410; 713; 198; 660; 269]});;                                                               
let _ = assert(times (from_int 0) (from_int 0) = from_int 0);;
let _ = assert(times (from_int 0) (from_int 1) = from_int 0);;
let _ = assert(times (from_int (-1)) (from_int 0) = from_int 0);;
let _ = assert(times (from_int 200) (from_int 1000) = from_int 200000);;
let _ = assert(times (from_int 1000) (from_int 200) = from_int 200000);;
let _ = assert(times (from_int (-200)) (from_int 1000) = from_int (-200000));;
let _ = assert(times (from_int 1000) (from_int (-200)) = from_int (-200000));;
let _ = assert(times (from_int 200) (from_int (-1000)) = from_int (-200000));;
let _ = assert(times (from_int (-1000)) (from_int 200) = from_int (-200000));;
let _ = assert(times (from_int (-1000)) (from_int (-200)) = from_int 200000);;
let _ = assert(times (from_int (-200)) (from_int (-1000)) = from_int 200000);;
