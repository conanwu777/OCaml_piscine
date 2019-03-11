let fibonacci n =
	let rec loop a b n =
		if n < 0 then -1
		else if n == 0 then a
		else if n == 1 then b
		else
			let c = a + b in
				loop b c (n - 1) 
	in
		loop 0 1 n

let () =
	let n =
		read_int ()
	in
		print_int (fibonacci n);
		print_char '\n'
