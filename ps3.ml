(*
                         CS 51 Problem Set 2
                           Bignums and RSA
                             Spring 2018
*)

(*======================================================================
Section 1: Bignums 

In this problem set, as with the previous one, you may express your
solution to a particular problem in terms of another function from a
previous problem. Furthermore, you may use functions from the List
module where appropriate.
......................................................................*)

(* bignum: Type for representing bignums (arbitrary precision
   integers. Uses a boolean field neg for negative numbers and a list
   coeffs of coefficients from most to least significant. *)
   
type bignum = {neg: bool; coeffs: int list} ;;

(* cBASE: Global constant, the base for representing bignums, which
   must be a power of 10. Your code should work for any reasonable
   value of cBASE, not just the initial value we've set it to. When
   submitting, have cBASE be 1000. *)
let cBASE = 1000 ;;
  
(*......................................................................
Problem 1: Negation
......................................................................*)
  
let negate (b : bignum) : bignum =
  match b with
  | {neg = false; coeffs = []} -> b
  | {neg = x; coeffs = y} -> {neg = not x; coeffs = y} ;;

(*......................................................................
Problem 2: Comparing bignums
......................................................................*)  

let equal (b1 : bignum) (b2 : bignum) : bool =
  b1 = b2 ;;

let less (b1 : bignum) (b2 : bignum) : bool =
  let nonneg_comparer lst1 lst2 =
  if List.compare_lengths lst1 lst2 = -1 then true 
  else if List.compare_lengths lst1 lst2 = 1 then false 
  else lst1 < lst2 in 
  match b1.neg, b2.neg with
  | false, true -> false
  | true, false -> true
  | false, false ->
      nonneg_comparer b1.coeffs b2.coeffs 
  | true, true ->
      not ((nonneg_comparer b1.coeffs b2.coeffs) || (equal b1 b2)) ;; 

let greater (b1 : bignum) (b2 : bignum) : bool =
  less b2 b1 ;;

(*......................................................................
Problem 3: Converting to and from bignums
......................................................................*)

let from_int (n : int) : bignum =
  let rec lister lst i =
  if i < cBASE then i :: lst
  else lister ((i mod cBASE) :: lst) (i / cBASE) in 
  if n = 0 then {neg = false; coeffs = []}
  else if n = min_int then 
    {neg = true; coeffs = [4; 611; 686; 18; 427; 387; 904]} 
  else if n < 0 then {neg = true; coeffs = lister [] (- n)}
  else {neg = false; coeffs = lister [] n} ;;

let to_int (b : bignum) : int option =
  let rec converter i lst =
  match List.rev lst with
  | [] -> 0
  | hd :: tl -> hd * i + converter (i * cBASE) (List.rev tl) in
  if ((greater b (from_int max_int)) || (less b (from_int min_int))) then None 
  else match b with
  | {neg = false; coeffs = co_lst} -> Some (converter 1 co_lst)
  | {neg = true; coeffs = co_lst} -> Some (converter (-1) co_lst) ;;

(*......................................................................
Helpful functions (not to be used in problems 1 to 3)
......................................................................*)

(* strip_zeroes -- Removes zero coefficients from the beginning of the
   coefficients part of a bignum representation *)
let rec strip_zeroes (b : int list) : int list =
  match b with
  | 0 :: t -> strip_zeroes t
  | _ -> b ;;

(* clean -- Removes zero coefficients from the beginning of a bignum
   representation *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = strip_zeroes b.coeffs} ;;

(* rand_bignum -- Returns a random bignum from 0 to bound (inclusive).
   You might use this to help randomly test functions. *)
let rand_bignum (bound: bignum) =
  let randbase = List.map (fun _ -> Random.int cBASE) in
  let rec rand_bignum_rec (bound: int list) =
    match bound with
    | [] -> []
    | h::t -> let r = Random.int (h+1) in
              r::((if r = h then rand_bignum_rec else randbase) t)
  in {neg = false; coeffs = strip_zeroes (rand_bignum_rec bound.coeffs)} ;;
       
(* explode -- Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s 1 (len - 1)) ;;

(* implode -- Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
  | [] -> ""
  | c :: t -> String.make 1 c ^ implode t ;;
                                          
(* take_first -- Returns the first n elements of list l (or the whole
   list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
  | [] -> []
  | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1) ;;

(* split -- Returns a pair (first n elements of lst, rest of elements
   of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
  | [] -> ([], [])
  | h :: t -> let (lst1, lst2) = split t (n - 1) in
              (h :: lst1, lst2) ;;

(* intlog -- Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  int_of_float (log10 (float_of_int base)) ;;

(* from_string -- Converts a string representing an integer to a
   bignum. Assumes the base is a power of 10. *)
let from_string (s : string) : bignum =
  let rec from_string_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog cBASE) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: from_string_rec rest
  in
  match explode s with
  | [] -> from_int 0
  | h :: t ->
      if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (from_string_rec (List.rev t)))}
      else {neg = false;
            coeffs =
              (strip_zeroes (List.rev (from_string_rec (List.rev (h :: t)))))}

(* to_string -- Converts a bignum to its string representation.
   Returns a string beginning with ~ for negative integers. Assumes
   the base is a power of 10. *)
let to_string (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s
    else "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s 1 (String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
    | [] -> ""
    | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog cBASE)
                ^ coeffs_to_string t in
  let stripped = strip_zeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs ;;

(*......................................................................
Arithmetic functions
......................................................................*)

(* plus_pos -- Returns a bignum representing b1 + b2.  Assumes that the
   sum is positive. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
    : bool * int list =
    match (coeffs1, coeffs2) with
    | ([], []) -> pair_from_carry carry
    | ([], _) ->
        if carry = 0 then (neg2, coeffs2)
        else plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
    | (_, []) ->
        if carry = 0 then (neg1, coeffs1)
        else plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
    | (h1 :: t1, h2 :: t2) ->
        let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
        let result = h1 * sign1 + h2 * sign2 + carry in
        if result < 0 then
          let (negres, coeffsres) =
              plus_with_carry (neg1, t1) (neg2, t2) (-1)
          in (negres, result + cBASE :: coeffsres)
        else if result >= cBASE then
          let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
          in (negres, result - cBASE :: coeffsres)
        else
          let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
          in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
      plus_with_carry (b1.neg, List.rev b1.coeffs)
                      (b2.neg, List.rev b2.coeffs)
                      0
  in {neg = negres; coeffs = strip_zeroes (List.rev coeffsres)} ;;

(*......................................................................
Problem 4

The plus function returns a bignum representing b1 + b2. However,
it does NOT make the same assumption as plus_pos. 

Hint: How can you use plus_pos to implement plus? Make sure that
your implementation preserves the bignum invariant.
......................................................................*)

let plus (b1 : bignum) (b2 : bignum) : bignum =
  let switch = ((b1.neg = true && b2.neg = true) || 
                (b1.neg = true && greater (negate b1) b2) || 
                (b2.neg = true && less b1 (negate b2))) in
  if switch then (negate (plus_pos (negate b1) (negate b2)))
  else plus_pos b1 b2 ;;

(*......................................................................
Problem 5

The times function returns a bignum representing b1 * b2. Think about
how you were first taught multiplication:

      543 
    x 242
    -----
     1086
  + 21720 <--- Note that a zero is appended after the partial product
 + 108600 <--- Note that two zeroes are appended after the partial product
 --------
= 131,406  

When approaching this problem, it is advisable to break the problem
down into simpler, easier-to-implement sub-problems. That way, if a
bug arises in your code, you can test each helper function
individually rather than having to test all of it at once.

You may assume positivity in some of your helper functions if it 
simplifies the code, as long as the invariant is preserved. 
......................................................................*)

let times (b1 : bignum) (b2 : bignum) : bignum =
  let rec helper lst n carry = 
  match lst with
  | [] -> if carry > 0 then [carry] else []
  | hd :: tl -> 
      let fullprod = hd * n + carry in
      (fullprod mod cBASE) :: (helper tl n (fullprod / cBASE)) in
  let rec add_zero numeral mult_list = 
      if numeral > 0 then add_zero (numeral - 1) (0 :: mult_list)
      else (List.rev mult_list) in     
  let rec multiplication num lst1 lst2 =
  match List.rev lst1, List.rev lst2 with
  | [], _ -> from_int 0
  | _, [] -> from_int 0
  | (h1 :: t1), y -> 
      let new_lst = add_zero num (helper y h1 0) in
      plus {neg = false; coeffs = new_lst} (multiplication (num + 1) t1 y) in 
  if b1.neg = b2.neg then multiplication 0 b1.coeffs b2.coeffs
  else negate (multiplication 0 b1.coeffs b2.coeffs) ;;

(*======================================================================
Section 2: Challenge Problem - RSA Cryptosystem

A reminder: these problems are not required and are only for 
your karmic edification. You should feel free to do these after 
you've done your best work on your primary part of the problem set.

Found below is support code for RSA. Hint: each part of this 
problem can be implemented in approximately one line of code. 
......................................................................*)

(* div_sing -- Divides a bignum by a single digit, that is, returns a
   bignum representing b/n, where n is an integer less than cBASE *)
let div_sing (b : int list) (n : int) : int list * int =
  let rec div_sing_rec (b : int list) (r : int) : int list * int =
    match b with
    | [] -> [], r
    | h :: t ->
        let dividend = r * cBASE + h in
        let quot = dividend / n in
        let (q, r) = div_sing_rec t (dividend-quot * n) in
        (quot :: q, r)
  in
  match b with
  | [] -> [], 0
  | [a] -> [a / n], a mod n
  | h1 :: h2 :: t -> if h1 < n then div_sing_rec (h1 * cBASE + h2 ::t) 0
                     else div_sing_rec b 0 ;;

(* div_mod -- Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let div_mod (b1 : bignum) (b2 : bignum): bignum * bignum =
  let rec div_mod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m)
    else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
      | [] -> failwith "Division by zero"
      | ns :: _ -> let (p, _) =
          if ns + 1 = cBASE then
            (take_first mc (List.length mc - List.length nc), 0)
          else
            let den = ns + 1 in
            let num = take_first mc (List.length mc - List.length nc + 1) in
            div_sing num den
        in
        let bp = clean {neg = false; coeffs = p} in
        let p2 = clean (if equal bp (from_int 0) then from_int 1 else bp) in
        div_mod_rec (clean (plus m (negate (times n p2))))
                   (clean n)
                   (clean (plus psum p2))
  in
  div_mod_rec (clean b1) (clean b2) (from_int 0) ;;

(* exp_mod -- Returns b to the power of e mod m *)
let rec exp_mod (b : bignum) (e : bignum) (m : bignum) : bignum =
  if equal e (from_int 0) then from_int 1
  else if equal e (from_int 1) then
    snd (div_mod (clean b) (clean m))
  else
    let (q, r) = div_mod (clean e) (from_int 2) in
    let res = exp_mod (clean b) q (clean m) in
    let (_, x) = div_mod (times (times res res) (exp_mod (clean b) r (clean m)))
                        (clean m) in
    {neg = x.neg; coeffs = strip_zeroes x.coeffs} ;;

(* exponent -- Returns b to the power of e *)
let rec exponent (b : bignum) (e : bignum) : bignum =
  if equal (clean e) (from_int 0) then from_int 1
  else if equal (clean e) (from_int 1) then clean b
  else
    let (q, r) = div_mod (clean e) (from_int 2) in
    let res = exponent (clean b) q in
    let exp = (times (times res res) (exponent (clean b) r))
    in {neg = exp.neg; coeffs = strip_zeroes exp.coeffs} ;;

(* is_prime -- Returns true if n is prime, false otherwise. *)
let is_prime (n : bignum) : bool =
  let rec miller_rabin (k : int) (d : bignum) (s : int) : bool =
    if k < 0 then true else
    let rec square (r : int) (x : bignum) =
      if r >= s then false else
      let x = exp_mod x (from_int 2) n in

        if equal x (from_int 1) then false
        else if equal x (plus n (from_int (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (rand_bignum (plus n (from_int (-4)))) (from_int 2) in
    let x = exp_mod a d n in
      if equal x (from_int 1) || equal x (plus n (from_int (-1))) then
        miller_rabin (k - 1) d s
      else square 1 x 
  in
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n : bignum) (s : int) =
    let (q, r) = div_mod n (from_int 2) in
      if equal r (from_int 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = div_mod n (from_int 2) in
    if equal r (from_int 0) then false else
      let (d, s) = factor (plus n (from_int (-1))) 0 in
        miller_rabin 20 d s ;;

(* euclid -- Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m : bignum) (d : bignum) : bignum * bignum * bignum =
  if equal d (from_int 0) then (from_int 1, from_int 0, m)
  else
    let (q, r) = div_mod m d in
    let (s, t, g) = euclid d r in
      (clean t, clean (plus s (negate (times q t))), clean g) ;;

(* generate_random_prime -- Generate a random prime number between min
   and max-1 (inclusive) *)
let rec generate_random_prime (min : bignum) (max: bignum) : bignum =
  let rand = plus (rand_bignum (plus max (negate min))) min in
    if is_prime rand then rand else generate_random_prime min max ;;


(*......................................................................
  Code for encrypting and decrypting messages using RSA 
 *)

(* generate_key_pair -- Generate a random RSA key pair, returned as (e,
   d, n).  p and q will be between 2^n and 2^(n+1).  Recall that (n,
   e) is the public key, and (n, d) is the private key. *)
  
let rec generate_key_pair (r : bignum) : bignum * bignum * bignum =
  let c1 = from_int 1 in
  let c2 = from_int 2 in
  let p = generate_random_prime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generate_random_prime (exponent c2 r) (exponent c2 (plus r c1)) in
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  let rec selectPair () =
    let e = generate_random_prime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (clean e, clean d, clean (times p q))
      else selectPair ()
  in
    if equal p q then generate_key_pair r else selectPair () ;;

(*......................................................................
Challenge Problem 6: Encrypting and decrypting bignums
......................................................................*)

(* encrypt_decrypt_bignum -- Encrypt or decrypt a bignum. To encrypt,
   pass in the arguments n e s. To decrypt, pass in the inputs n d
   s. *)
let encrypt_decrypt_bignum (n : bignum) (e : bignum) (s : bignum) : bignum =
  failwith "encrypt_decrypt_bignum not implemented" ;;

(* chars_to_bignums -- Pack a list of chars as a list of bignums, with m
   chars to a bignum. *)
let rec chars_to_bignums (lst : char list) (m : int) : bignum list =
  let rec encchars lst =
    match lst with
    | [] -> (from_int 0)
    | c :: t -> clean (plus (times (encchars t) (from_int 256))
                            (from_int (int_of_char c)))
  in
    match lst with
    | [] -> []
    | _ -> let (enclist, rest) = split lst m in
           encchars enclist :: chars_to_bignums rest m

(* bignums_to_chars -- Unpack a list of bignums into chars (reverse of
   chars_to_bignums) *)
let rec bignums_to_chars (lst : bignum list) : char list =
  let rec decbignum b =
    if equal b (from_int 0) then []
    else let (q, r) = div_mod b (from_int 256) in
      match to_int r with
      | None -> failwith "bignums_to_chars: representation invariant broken"
      | Some ir -> char_of_int ir :: decbignum q
  in
    match lst with
    | [] -> []
    | b :: t -> decbignum b @ bignums_to_chars t

(* bytes_in_key -- Return the number of bytes required to represent an
   RSA modulus. *)
let bytes_in_key (n : bignum) =
  int_of_float (float_of_int (List.length (strip_zeroes n.coeffs) - 1)
                *. log10 (float_of_int cBASE) /. (log10 2. *. 8.))

(* Encrypts or decrypts a list of bignums using RSA. To encrypt, pass
   in n e lst. To decrypt, pass in n d lst. *)
let rec encrypt_decrypt_bignums (n : bignum) (e : bignum) (lst : bignum list) =
  match lst with
  | [] -> []
  | h :: t -> encrypt_decrypt_bignum n e h :: encrypt_decrypt_bignums n e t

(*......................................................................
Challenge Problem 7: Encrypting and decrypting strings
......................................................................*)                                        

let encrypt (n : bignum) (e : bignum) (s : string) =
  failwith "encrypt not implemented" ;;

(* 
Decrypt an encrypted message (list of bignums) to produce the
original string. 
*)
let decrypt (n : bignum) (d : bignum) (m : bignum list) =
  failwith "decrypt not implemented" ;;

(*======================================================================
Section 3: challenge Problem - Faster Multiplication 
......................................................................*)

(*......................................................................
Challenge Problem 8: Faster bignum multiplication

The function "times_faster" returns a bignum representing b1 * b2. 
......................................................................
*)

let times_faster (b1 : bignum) (b2 : bignum) : bignum =
  failwith "times_faster not implemented" ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = 600 ;;
