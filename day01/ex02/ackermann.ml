let rec ackermann m n =
	if m < 0 || n < 0 then -1
	else if m == 0 then n + 1
	else if n == 0 then ackermann (m - 1) 1
	else
		ackermann (m - 1) (ackermann m (n - 1))

let () =
	let m = read_int () in
	let n = read_int () in
		print_int (ackermann m n);
		print_char '\n'
