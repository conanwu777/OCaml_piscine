let ft_test_sign x =
	if x < 0
	then print_endline "negative"
	else print_endline "positive"

let () = 
	let n = read_int () in
		ft_test_sign n