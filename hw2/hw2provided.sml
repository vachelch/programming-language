(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 1-a *)
fun all_except_option(x: string, ys: string list) = 
	let
		fun idx(x: string, ys: string list, acc: int) =
			case ys of 
				[] => ~1
			  | y::ys_ => if same_string(x, y) then acc else idx(x, ys_, acc + 1)

		fun helper(ys: string list, idx: int) = 
			case ys of
				[] => []
			  | y::ys_ => if idx = 0 then helper(ys_, idx-1) else y::helper(ys_, idx-1)
		val pos = idx(x, ys, 0)
	in 
		if pos = ~1 then NONE else SOME (helper(ys, pos))
	end

(* 1-b *)
fun get_substitutions1(subs: string list list, s: string) = 
	case subs of 
		[] => []
	  | sub::subs_ => 
			case all_except_option(s, sub) of
				NONE => get_substitutions1(subs_, s)
			  | SOME res => res @ get_substitutions1(subs_, s)

(* 1-c *)
fun get_substitutions2(subs: string list list, s: string) = 
	let 
		fun helper(subs: string list list, s: string, acc: string list) = 
			case subs of
				[] => acc
			  | sub::subs_ => 
					case all_except_option(s, sub) of
						NONE => helper(subs_, s, acc)
					  | SOME res => helper(subs_, s, acc @ res)
	in
		helper(subs: string list list, s: string, [])
	end

(* 1-d *)
fun similar_names(subs: string list list, {first=f, middle=m, last=l}) = 
	let
		fun ans(fs: string list) = 
			case fs of 
				[] => []
				| f::fs_ => {first=f, middle=m, last=l}::ans(fs_)
		val fs = f::(get_substitutions2(subs, f))
	in
		ans(fs)
	end

(* 2-a *)
fun card_color(cd: card) =
	case cd of 
		(Clubs, _) => Black
		| (Spades, _) => Black
		| (Diamonds, _) => Red
		| (Hearts, _) => Red

(* 2-b *)
fun card_value(cd: card) = 
	case cd of
		(_, Num i) => i
		| (_, Ace) => 11
		| _ => 10

(* 3-c *)
fun remove_card(cds: card list, cd: card, e) = 
	let 
		fun all_card_except(cds: card list) = 
			case cds of 
				[] => raise e
				| x::xs => if x = cd then xs else x::all_card_except(xs)
	in 
		all_card_except(cds)
	end

(* 3-d *)
fun all_same_color(cds: card list) = 
	let
		fun is_same_color(cd: card, cds: card list) =
			case cds of
				[] => true
				| c::cds_ => card_color(cd) = card_color(c) andalso is_same_color(cd, cds_)
	in 
		case cds of 
			[] => true
			| cd::cds_ => is_same_color(cd, cds_)
	end

(* 3-e *)
fun sum_cards(cds: card list) = 
	let 
		fun sum(cds: card list, acc: int) = 
			case cds of 
				[] => acc
				| x::xs => sum(xs, acc + card_value(x))
	in
		sum(cds, 0)
	end

(* 3-f*)
fun score(cds: card list, goal: int) = 
	let 
		val isSameColor = all_same_color(cds)
		val sum = sum_cards(cds)
	in
		if sum > goal then
			if isSameColor then 3 * (sum - goal) div 2 else 3 * (sum - goal)
		else
			if isSameColor then (goal - sum) div 2 else (goal - sum)
	end

fun officiate(cds: card list, moves: move list, goal: int) =
	let
		fun play(cds: card list, moves: move list, holds: card list) =
			case moves of
				[] => holds
				| Draw::mvs => (
					case cds of
						[] => holds
						| cd::cds_ => 
							if sum_cards(cd::holds) >= goal then
								cd::holds
							else
								play(cds_, mvs, cd::holds))
				| (Discard cd)::mvs => play(cds, mvs, remove_card(holds, cd, IllegalMove))
		val res = play(cds, moves, [])
	in
		score(res, goal)
	end

(*
val test1a_1 = all_except_option("abc", ["aaa", "abc", "bbb"]) = SOME ["aaa", "bbb"]
val test1a_2 = all_except_option("abc", ["aaa", "bbb"]) = NONE
val test1a_3 = all_except_option("abc", ["abc"]) = SOME []

val test1b_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test1b_2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = 
["Jeffrey","Geoff","Jeffrey"]
val test1b_3 = get_substitutions1([], "Jeff") = 
[]

val test1c_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test1c_2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test1c_3 = get_substitutions2([], "Jeff") = 
[]

val test1d_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"},
                 {first="Fredrick", last="Smith", middle="W"},
                 {first="Freddie", last="Smith", middle="W"},
                 {first="F", last="Smith", middle="W"}]

val test2a_1 = card_color(Clubs, Num 1) = Black

val test2b_1 = card_value(Clubs, Num 1) = 1
val test2b_2 = card_value(Clubs, Ace) = 11
val test2b_3 = card_value(Clubs, King) = 10

val test2c_1 = remove_card([(Clubs, King), (Clubs, Queen), (Clubs, King)], (Clubs, King), IllegalMove) = [(Clubs, Queen), (Clubs, King)]

val test2d_1 = all_same_color([(Clubs, King), (Clubs, Queen), (Spades, King)]) = true
val test2d_2 = all_same_color([(Clubs, King), (Hearts, Queen), (Spades, King)]) = false
val test2d_3 = all_same_color([(Clubs, King)]) = true

val test2e_1 = sum_cards([(Clubs, King), (Clubs, Queen), (Spades, King)]) = 30

val test2f_1 = score([(Clubs, King), (Clubs, Queen), (Spades, King)], 30) = 0
val test2f_2 = score([(Clubs, King), (Clubs, Queen), (Spades, King)], 15) = 22
val test2f_3 = score([(Clubs, King), (Clubs, Queen), (Spades, King)], 35) = 2

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
*)