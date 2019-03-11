let ft_sum f a b =
	if b < a then nan
	else
		let rec recur f a b cur =
			if b < a then cur
			else recur f (a + 1) b (cur +. (f a))
		in
			recur f a b (float_of_int 0)

let () =
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
	print_char '\n';
	print_float (ft_sum (fun i -> 1.0 /. float_of_int (i * i)) 1 1000);
	print_char '\n';
	print_float (3.1415926 *. 3.1415926 /. 6.0);
	print_char '\n'
