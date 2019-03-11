let ft_rot_n n s =
	let rot_char c = 
		if c >= 'a' && c <= 'z'
			then char_of_int (int_of_char 'a' + (((int_of_char c - int_of_char 'a') + n) mod 26))
		else if c >= 'A' && c <= 'Z'
			then char_of_int (int_of_char 'A' + (((int_of_char c - int_of_char 'A') + n) mod 26))
		else c
	in
	String.map rot_char s

let () = 
	let n = read_int () in
	let s = read_line () in
		print_endline (ft_rot_n n s)
