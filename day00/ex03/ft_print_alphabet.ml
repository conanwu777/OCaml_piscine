let ft_print_alphabet () =
	let st = int_of_char 'a' in
		let ed = int_of_char 'z' in
			let rec loop x = 
				if x <= ed then
				begin
					print_char (char_of_int x);
					loop (x + 1)
				end
			in
				loop st;
				print_char '\n'

let () = 
	ft_print_alphabet ()
