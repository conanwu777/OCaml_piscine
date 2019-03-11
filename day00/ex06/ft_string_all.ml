let is_digit c =
	if c < '0' then false
	else if c > '9' then false
	else true

let ft_string_all f s =
	let n = String.length s in
	let rec loop i s =
	if i >= n then true
	else if f (String.get s i) == false then false
	else loop (i + 1) s in
		loop 0 s

let () = 
	let s = read_line () in
		if ft_string_all is_digit s == true then
			print_endline "true"
		else
			print_endline "false"
