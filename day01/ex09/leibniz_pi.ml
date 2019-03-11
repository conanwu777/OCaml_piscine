let ft_sum f a b =
	if b < a then nan
	else
		let rec recur f a b cur =
			if b < a then cur
			else recur f (a + 1) b (cur +. (f a))
		in
			recur f a b (float_of_int 0)

let f i =
	4. *. (-1.) ** (float_of_int i) /. (2. *. float_of_int i +. 1.)

let abs x =
	if x > 0. then x
	else (-1. *. x)

let leibniz_pi d =
	let pi = 4.0 *. (atan 1.) in
	let rec recur i d cur pi =
		if abs (cur -. pi) < d then i
		else
			recur (i + 1) d (cur +. (f i)) pi
	in
		recur 0 d 0. pi

let () =
	print_float (ft_sum f 0 (leibniz_pi 1.));
	print_char '\n';
	print_float (ft_sum f 0 (leibniz_pi 0.1));
	print_char '\n';
	print_float (ft_sum f 0 (leibniz_pi 0.01));
	print_char '\n';
	print_float (ft_sum f 0 (leibniz_pi 0.001));
	print_char '\n';
	print_float (ft_sum f 0 (leibniz_pi 0.0001));
	print_char '\n';
	print_float (ft_sum f 0 (leibniz_pi 0.00001));
	print_char '\n'
