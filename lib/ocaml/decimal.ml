(* Some sort of temporary decimal representation until I implement a full
   Decimal type that corresponds to the standard:

   http://speleotrove.com/decimal/

*)


(* Utility string functions. *)
let createini len c =
  let s = String.create len in
  let () = String.fill s 0 len c in s;;


(* Precision settings (hard-coded for now). *)
let _mult = 10000000;;
let _ndigits = 8;;


type decimal = Decimal of int;;



(* Internal function to convert a string into an integer and a fraction *)
let comps_of_string s =
  let len = String.length s in
  let (i, f) = try
    let idx = String.index s '.' in
      (String.sub s 0 idx, String.sub s (idx+1) (len-idx-1))
  with
      Not_found -> (s, "")
  in
  let safe_iofs s =
    if String.length s > 0 then int_of_string s
    else 0
  in
  let padded_iofs s =
    let ns = createini _ndigits '0' in
      String.blit s 0 ns 0 (String.length s);
      safe_iofs ns
  in
    (safe_iofs i, padded_iofs f) ;;



(* Create a new decimal object. *)
let create s =
  let ip, fp = comps_of_string s in
    Decimal(ip, fp);;


(* Convert a decimal object to a string representation. *)
let string_of_decimal = function |
FIXME this is obviously flawed on the fraction side, need to pad and strip
    Decimal(ip, fp) -> string_of_int ip ^ "." ^ string_of_int fp;;




let (+^) d1 d2 = match d1, d2 with
    Decimal(ip1, fp1), Decimal(ip2, fp2) ->
      (ip1+ip2, fp1+fp2);;



(* let (+d) Decimal()d1 d2 =  *)


(* string_of_decimal (create "400.232");; *)
;;

let d1 = create "0.0017" and
    d2 = create "1.032" in
  d1 +^ d2;;

string_of_decimal (create "0.0017");;


