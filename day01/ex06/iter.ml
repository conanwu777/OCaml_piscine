let rec iter f x n =
	if n < 0 then -1
	else if n == 0 then x
	else
		iter f (f x) (n - 1)

let () =
	print_int (iter (fun x -> x * x) 2 4);
	print_char '\n';
	print_int (iter (fun x -> 2 * x) 2 4);
	print_char '\n';
