let rec expand = function
	| [] -> []
	| head::other -> 
		begin
			let s1 = ("0" ^ head) in
			let s2 = ("1" ^ head) in
			let l1 = s1::(expand other) in
				l1@(s2::[])
		end

let rec gray_recur n = match n with
	| 0 -> []
	| 1 -> "0"::"1"::[]
	| _ -> expand (gray_recur (n - 1))

let rec print_list = function
	| [] -> ()
	| head::second::other ->
		begin
			print_string (head ^ " ");
			print_list (second::other)
		end
	| head::other ->
		print_string (head ^ "\n")

let gray n =
	if n < 0 then
		print_endline "Error"
	else
		print_list (gray_recur n)

let () =
	let n = read_int () in
		gray n
