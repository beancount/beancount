(* Test implementation of a Wallet using an assoc list *)



type wallet = (string * float) list;;



let wadd w1 w2 =
  let rec iadd l1 l2 acc = 
    match (l1, l2) with
	([], []) -> List.rev acc
      | ([], y::ys) -> iadd l1 ys (y::acc)
      | (x::xs, []) -> iadd xs l2 (x::acc)
      | ((cx, ax)::xs, (cy, ay)::ys) when cx=cy -> iadd xs ys ((cx,(ax+.ay))::acc)
      | ((cx, ax)::xs, (cy, ay)::ys) when cx<cy -> iadd xs ys ((cx,(ax+.ay))::acc)
  in
    iadd w1 w2 [];;
	  
  

let w1 = [("USD", 1200.)] and
    w2 = [("CAD", 12.232)] in
  wadd w1 w2;;


let w1 = [("USD", 1200.)] and
    w2 = [("USD", 12.232)] in
  wadd w1 w2;;



