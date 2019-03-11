let rec recur d1 d2 d3 =
	print_int d1;
	print_int d2;
	print_int d3;
	if d1 < 7 then
	begin
		print_string ", ";
		if d3 < 9 then
			recur d1 d2 (d3 + 1)
		else if d2 < 8 then
			recur d1 (d2 + 1) (d2 + 2)
		else
			recur (d1 + 1) (d1 + 2) (d1 + 3)
	end
	else
		print_string "\n"

let ft_print_comb () =
	recur 0 1 2

let () = 
	ft_print_comb ()
