let rec check a l2 = match l2 with
	| [] -> false
	| head::other ->
		begin
			if a == head then true
			else (check a other)
		end

let rec crossover l1 l2 = match l1 with
	| [] -> []
	| head::other ->
		begin
			if check head l2 then head::(crossover other l2)
			else crossover other l2
		end


let rec print_list l = match l with
	| [] -> ()
	| head::other ->
		begin
		print_int head;
		print_list other
		end

let () =
	let l1 = 1::2::3::4::5::6::7::8::[] in
	let l2 = 9::7::8::3::1::[] in
	begin
		print_list (crossover l1 l2);
		print_char '\n'
	end
