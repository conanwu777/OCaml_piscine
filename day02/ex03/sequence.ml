let rec count l = match l with
	| [] -> 0
	| first::second::other ->
		if first == second then 1 + count (second::other)
		else 1
	| first::other -> 1

let rec chop c l = match l with
	| [] -> []
	| head::other ->
		if c == head then chop c other
		else l

let rec read_list l = match l with
	| [] -> []
	| head::other -> (count l)::head::(read_list (chop head l))

let rec find_list n = match n with
	| 1 -> 1::[]
	| _ -> read_list (find_list (n - 1))

let sequence n =
	if n <= 0 then ""
	else
		let l = find_list n in
		let rec list_to_string l = match l with
			| [] -> ""
			| head::other -> (string_of_int head) ^ (list_to_string other)
		in
			list_to_string l

let () =
	let n = read_int () in
		print_endline (sequence n)
