let rec ft_power x n =
	if n <= 0 then 1
	else
		x * ft_power x (n - 1)

let () = 
	let x = read_int() in
		let n = read_int() in
			begin
				print_int (ft_power x n);
				print_char '\n'
			end
