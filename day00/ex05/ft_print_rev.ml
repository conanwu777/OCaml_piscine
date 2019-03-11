let ft_print_rev s =
	let n = String.length s in
	let rec loop s n =
	if n >= 0 then
		begin
			print_char (String.get s n);
			loop s (n - 1)
		end
	in
	loop s (n - 1);
	print_char '\n'

let () = 
	let s = read_line () in
		ft_print_rev s
