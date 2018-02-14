(*
	             CS 51 Problem Set 2
	               Bignums and RSA
	                 Spring 2018
*)

type bignum = {neg: bool; coeffs: int list}

val cBASE : int

val negate : bignum -> bignum
val equal : bignum -> bignum -> bool
val less : bignum -> bignum -> bool
val greater : bignum -> bignum -> bool
val from_int : int -> bignum
val to_int : bignum -> int option

val from_string : string -> bignum

val plus : bignum -> bignum -> bignum
val times : bignum -> bignum -> bignum

val generate_key_pair : bignum -> bignum * bignum * bignum

val encrypt_decrypt_bignum : bignum -> bignum -> bignum -> bignum
val encrypt : bignum -> bignum -> string -> bignum list
val decrypt : bignum -> bignum -> bignum list -> string
val times_faster : bignum -> bignum -> bignum

val minutes_spent_on_pset : unit -> int