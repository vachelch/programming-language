(* 1 *)
fun is_older(date1: int*int*int, date2: int*int*int) =
    let val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1

        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        if y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2) then
            true
        else
            false
    end

(* 2 *)
fun number_in_month(dates: (int*int*int) list, month: int) = 
	if null dates then
		0
	else
		let val cur = #2 (hd dates)
		in if cur = month then 1 + number_in_month(tl dates, month) else number_in_month(tl dates, month)
		end


(* 3 *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
	if null months then
		0
	else
		number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4 *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
	if null dates then
		[]
	else
		let val cur = #2 (hd dates)
		in 
			if cur = month then 
				hd dates :: dates_in_month(tl dates, month)
			else
				dates_in_month(tl dates, month)
		end

(* 5 *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
	if null months then
		[]
	else
		dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6 *)
fun get_nth(strs :string list, n: int) = 
	if n = 1 then
		hd strs
	else
		get_nth(tl strs, n-1)

(* 7 *)
fun date_to_string(date: int*int*int) = 
	let 
		val ms = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
		val y = Int.toString(#1 date)
		val m = get_nth(ms, #2 date)
		val d = Int.toString(#3 date)
	in 
		m ^ " " ^ d ^ ", " ^ y
	end

(* 8 *)
fun number_before_reaching_sum(sum: int, nums: int list) = 
	if hd nums >= sum then
		0
	else
		1 + number_before_reaching_sum(sum - hd nums, tl nums)

(* 9 *)
fun what_month(num: int) = 
	let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in 
		number_before_reaching_sum(num, days) + 1
	end

(* 10 *)
fun month_range(left: int, right: int) = 
	if left > right then
		[]
	else
		what_month(left) :: month_range(left + 1, right)

(* 11 *)
fun oldest(dates: (int*int*int) list) = 
	if null dates then
		NONE
	else if null (tl dates) then
		SOME (hd dates)
	else
		let 
			val cur = hd dates
			val rest_oldest = oldest (tl dates)
		in
			if is_older(cur, valOf rest_oldest) then 
				SOME cur 
			else
				rest_oldest
		end

(* 12 *)
fun deduplicate(nums: int list) = 
	let 
		fun exist(needle: int, nums: int list) = 
			if null nums then
				false
			else
				if needle = hd nums then
					true 
				else 
					exist(needle, tl nums)
	in
		if null nums then
			[]
		else
			if exist (hd nums, tl nums) then
				deduplicate(tl nums)
			else 
				hd nums :: deduplicate(tl nums)
	end

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) = 
	number_in_months(dates, deduplicate(months))
fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) = 
	dates_in_months(dates, deduplicate(months))

(* 13 *)
fun index(nums: int list, idx: int) = 
	if idx = 1 then
		hd nums
	else
		index(tl nums, idx-1)

fun reasonable_date(date: int*int*int) =
	let
		val days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		val y = #1 date
		val m = #2 date
		val d = #3 date

		val yIsValid = y > 0
		val mIsValid = 0 < m andalso m < 13
		val dIsValidPre = (0 < d) andalso mIsValid andalso d <= index(days, m)
		
		fun leapYear(year: int) = 
			year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)

	in
		if yIsValid andalso mIsValid andalso dIsValidPre then
			if not (leapYear(y)) then
				m <> 2 orelse (m = 2 andalso d < 29)
			else
				true
		else
			false
	end

val test1_1 = is_older((2020,5,23), (2020,5,24)) = true
val test1_2 = is_older((2020,5,24), (2020,5,23)) = false
val test1_3 = is_older((2018,2,23), (2020,6,23)) = true
val test1_4 = is_older((2020,2,23), (2018,5,23)) = false

val test2_1 = number_in_month([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], 5) = 2
val test2_2 = number_in_month([(2020, 1, 2), (2020, 2, 2), (20, 5, 5)], 5) = 1
val test2_3 = number_in_month([(2020, 1, 2), (2020, 2, 2)], 5) = 0

val test3_1 = number_in_months([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], [5,1]) = 3
val test3_2 = number_in_months([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], [5,1,2]) = 4
val test3_3 = number_in_months([(2020, 1, 2)], [5,2]) = 0
val test3_4 = number_in_months([(2020, 1, 2)], [5,2]) = 0
val test3_5 = number_in_months([], [5,1,2]) = 0
val test3_6 = number_in_months([(2020, 1, 2)], []) = 0
val test3_7 = number_in_months([], []) = 0

val test4_1 = dates_in_month([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], 5) = [(2020, 5, 2), (20, 5, 5)]
val test4_2 = dates_in_month([(2020, 1, 2), (2020, 2, 2), (20, 5, 5)], 5) = [(20, 5, 5)]
val test4_3 = dates_in_month([(2020, 1, 2), (2020, 2, 2)], 5) = []

val test5_1 = dates_in_months([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], [5,1]) = [(2020, 5, 2), (20, 5, 5), (2020, 1, 2)]
val test5_2 = dates_in_months([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], [5,1,2]) = [(2020, 5, 2), (20, 5, 5), (2020, 1, 2), (2020, 2, 2)]
val test5_3 = dates_in_months([(2020, 1, 2)], [5,2]) = []
val test5_4 = dates_in_months([(2020, 1, 2)], [5,2]) = []
val test5_5 = dates_in_months([], [5,1,2]) = []
val test5_6 = dates_in_months([(2020, 1, 2)], []) = []
val test5_7 = dates_in_months([], []) = []

val test6_1 = get_nth(["abs", "abd", "baf"], 1) = "abs"
val test6_2 = get_nth(["abs", "abd", "baf"], 3) = "baf"


val test7_1 = date_to_string(2013, 1, 20) = "January 20, 2013"
val test7_2 = date_to_string(2011, 12, 1) = "December 1, 2011"

val test8_1 = number_before_reaching_sum(1, [1,2,3,4]) = 0
val test8_2 = number_before_reaching_sum(2, [1,2,3,4]) = 1
val test8_3 = number_before_reaching_sum(6, [1,2,3,4]) = 2

val test9_1 = what_month(1) = 1
val test9_2 = what_month(31) = 1
val test9_3 = what_month(32) = 2

val test10_1 = month_range(2,1) = []
val test10_2 = month_range(2,2) = [1]
val test10_3 = month_range(2,3) = [1,1]
val test10_4 = month_range(30,32) = [1,1,2]

val test11_1 = oldest([(2012,1,10), (2008, 8, 8), (2020, 5, 24)]) = SOME (2008, 8, 8)
val test11_2 = oldest([]) = NONE

val test12_Dup_1 = deduplicate([]) = []
val test12_Dup_2 = deduplicate([1,2,2,2,3,3,3,2,3]) = [1,2,3]
val test12_num_1 = number_in_months_challenge([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], [5,1,5,1]) = 3
val test12_dates_1 = dates_in_months_challenge([(2020, 1, 2), (2020, 2, 2), (2020, 5, 2), (20, 5, 5)], [5,1,5,1]) = [(2020, 5, 2), (20, 5, 5), (2020, 1, 2)]


val test13_1 = reasonable_date(2000, 2,29) = true
val test13_2 = reasonable_date(2008, 2,29) = true
val test13_3 = reasonable_date(3000, 2,29) = false
val test13_4 = reasonable_date(3000, 2,28) = true
val test13_5 = reasonable_date(3000, 15,400) = false
val test13_6 = reasonable_date(3000, 15,4) = false


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)


