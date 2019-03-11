let print_num n =
	print_int (n / 10);
	print_int (n mod 10)

let rec recur d1 d2 =
	print_num d1;
	print_char ' ';
	print_num d2;
	if d1 < 98 then
	begin
		print_string ", ";
		if d2 < 99
			then recur d1 (d2 + 1)
		else recur (d1 + 1) (d1 + 2)
	end
	else
		print_string "\n"

let ft_print_comb2 () =
	recur 0 1

let () = 
	ft_print_comb2 ()
