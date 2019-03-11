let repeat_string ?str:(x = "x") n =
	if n < 0 then "Error"
	else
	begin
		let s = "" in
		let rec recur x n s =
		if n == 0 then s
		else
		recur x (n - 1) (s ^ x)
		in
		recur x n s
	end

let () =
	print_endline (repeat_string 7);
	let str = read_line () in
	let n = read_int () in
		print_endline (repeat_string ~str:str n)
