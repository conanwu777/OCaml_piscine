let ft_is_palindrome s =
	if String.length s <= 1
		then true
	else
	begin
		let n = (String.length s) / 2 in
		let rec loop s x =
		if x > n
			then true
		else if String.get s x == String.get s (String.length s - 1 - x)
			then loop s (x + 1)
		else false
		in
		loop s 0		
	end

let () = 
	let s = read_line () in
		if ft_is_palindrome s == true then
			print_endline "true"
		else
			print_endline "false"
