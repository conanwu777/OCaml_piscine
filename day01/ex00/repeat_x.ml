let repeat_x ~n:n =
	if n < 0 then "Error"
	else
	begin
		let s = "" in
		let rec recur n s =
		if n == 0 then s
		else
		recur (n - 1) (s ^ "x")
		in
		recur n s
	end

let () =
	let n = read_int () in
		print_endline (repeat_x ~n:n)
