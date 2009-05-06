(* Lexer for Beancount syntax. *)

{
	(* header *)
}
let ident = regexp
	...
rule ruleset1 = parse
		regexp { action }
		| ...
		| regexp { action }
and ruleset2 = parse
		...
and ...
{
	(* trailer-and-end *)
} 
