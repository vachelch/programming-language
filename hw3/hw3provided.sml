(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals (strs: string list) = 
	List.filter (fn x => Char.isUpper(String.sub(x, 0))) strs

(* 2 *)
fun longest_string1 (strs: string list) = 
	foldl (fn (x, acc) => if (String.size x > String.size acc) then x else acc) "" strs

(* 3 *)
fun longest_string2 (strs: string list) = 
	foldl (fn (x, acc) => if (String.size x >= String.size acc) then x else acc) "" strs

(* 4 *)
fun longest_string_helper f strs =
	foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) "" strs

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = implode o rev o explode

(* 7 *)
fun first_answer f xs = 
	case xs of 
		[] => raise NoAnswer
		| x::xs_ => 
			case f(x) of
				NONE => first_answer f xs_
				| SOME v => v

(* 8 *)
fun all_answers f xs =
	let
		fun all_answers_acc (xs, acc) = 
			case xs of 
				[] => (
					case acc of 
						[] => NONE
						| acc_ => SOME acc_)
				| x::xs_ => (
					case f(x) of 
						NONE => all_answers_acc(xs_, acc)
						| SOME v => all_answers_acc(xs_, acc @ v))
	in 
		case xs of 
			[] => SOME []
			| xs_ => all_answers_acc(xs_, [])
	end

(* 9 *)
val count_wildcards = g (fn() => 1) (fn x => 0)
val count_wild_and_variable_lengths = g (fn() => 1) String.size
fun count_some_var(s: string, p: pattern) = 
	g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat (ptt: pattern) = 
	let 
		fun strs_in_patten (p: pattern) = 
			case p of
			    Wildcard          => []
			  | Variable x        => [x]
			  | TupleP ps         => List.foldl (fn (p,acc) => (acc @ (strs_in_patten p))) [] ps
			  | ConstructorP(_,p) => strs_in_patten p
			  | _                 => []
		fun norepeat (strs: string list) = 
			case strs of 
				[] => true
				| x::[] => true
				| x::xs => (List.all (fn y => y <> x) xs) andalso norepeat(xs)
	in
		norepeat(strs_in_patten ptt)
	end

(* 11 *)
fun match (val_patt: valu * pattern) = 
	case val_patt of 
		(Const v, ConstP p) => if v = p then SOME [] else NONE
      | (Unit, UnitP) => SOME []
      | (Tuple v, TupleP p) => if List.length v = List.length p then (all_answers match (ListPair.zip (v, p))) else NONE
      | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 then match (v, p) else NONE
      | (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | _ => NONE

(* 12 *)
fun first_match v ps =
	(SOME (first_answer (fn p => match(v, p)) ps)) handle NoAnswer => NONE


(*
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","BC"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 [] = ""
val test3_2 = longest_string2 ["A","bc","BC"] = "BC"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,2,3,4,5,6,7,1] = SOME [1,1]
val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Wildcard) = true
val test10_1 = check_pat (TupleP []) = true
val test10_2 = check_pat (Variable("x")) = true
val test10_3 = check_pat (TupleP [Variable("x")]) = true
val test10_4 = check_pat (TupleP [Variable("x"), Variable("x")]) = false
val test10_5 = check_pat (TupleP [Variable("x"), Variable("y"), Variable("x")]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (Tuple [Const(1), Unit], TupleP [Variable("x"), Variable("y")]) = SOME [("x",Const(1)), ("y", Unit)]

val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match Unit [ConstP 1] = NONE
val test12_2 = first_match Unit [ConstP 1] = NONE
val test12_3 = first_match (Tuple [Const(1), Unit]) [TupleP [Variable("x"), Variable("y")]] = SOME [("x",Const(1)), ("y", Unit)]
*)















