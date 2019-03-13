type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
	phosphate : phosphate;
	deoxyribose : deoxyribose;
	nucleobase : nucleobase
}

let generate_nucleotide c = 
	{
		phosphate = "phosphate";
		deoxyribose = "deoxyribose";
		nucleobase = match c with
			| 'A' -> A
			| 'T' -> T
			| 'C' -> C
			| 'G' -> G
			| _ -> None
	}

let () =
	let str = read_line () in
	let nu = generate_nucleotide (String.get str 0) in
		begin
			print_endline (nu.phosphate);
			print_endline (nu.deoxyribose);
			if nu.nucleobase == None then print_endline "None"
			else print_endline "Exist"
		end