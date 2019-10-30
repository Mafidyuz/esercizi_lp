let (-) text chars = 
	let rec meno_aux acc n = 
		match n with
		| n when n = String.length text -> acc
		| n when (String.contains chars text.[n]) -> meno_aux acc (n+1)
		| _ -> meno_aux (acc^(String.make 1 text.[n])) (n+1)
	in meno_aux "" 0;;
	

let is_palindrome s = 
	let rec is_palindrome_aux n = 
		match n with
		| n when n >= ((String.length s)/2) -> true
		| n when s.[n] = s.[(String.length s) - n - 1] -> is_palindrome_aux (n+1)
		| _ -> false
	in is_palindrome_aux 0;;

