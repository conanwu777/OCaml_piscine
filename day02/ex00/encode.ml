let rec encode l = match l with
	| [] -> []
	| head::second::other ->
		begin
			if head == second then
			let l2 = encode (second::other) in
			let add_one l2 = match l2 with
				| (n, c)::other -> ((n + 1), c)::other
				| _ -> []
			in
				add_one l2					
			else
				(1, head)::(encode (second::other))
		end
	| head::[] -> (1, head)::[]



let print_item t = match t with
	| (n, c) ->
		begin
			print_int n;
			print_char c
		end

let rec print_list l = match l with
	| [] -> ()
	| head::other ->
		begin
		print_item head;
		print_list other
		end

let () =
	let l = 'a'::'a'::'a'::'b'::'b'::'c'::'d'::'d'::[] in
	begin
		print_list (encode l);
		print_char '\n'
	end