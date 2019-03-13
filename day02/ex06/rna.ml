type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
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
			| 'U' -> U
			| _ -> None
	}

type helix = nucleotide list

let rec generate_helix n : helix =
	if n <= 0 then []
	else
		let t = Random.int 4 in
		let c = match t with
			| 0 -> 'A'
			| 1 -> 'T'
			| 2 -> 'C'
			| 3 -> 'G'
			| _ -> '.' in
		(generate_nucleotide c)::(generate_helix (n - 1))

let nb_to_str nb = match nb with
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| U -> "U"
	| None -> "."

let rec helix_to_string he = match he with
	| [] -> ""
	| head::other ->
		(nb_to_str head.nucleobase) ^ (helix_to_string other)

let rec complementary_helix he : helix = match he with
	| [] -> []
	| head::other ->
		let c = match head.nucleobase with
			| A -> 'T'
			| T -> 'A'
			| C -> 'G'
			| G -> 'C'
			| U -> '.'
			| None -> '.'
		in
			(generate_nucleotide c)::(complementary_helix other)

type rna = nucleobase list

let rec generate_rna he : rna = match he with
	| [] -> []
	| head::other ->
		begin
		let nb = match head.nucleobase with
			| A -> U
			| T -> A
			| C -> G
			| G -> C
			| U -> None
			| None -> None in
			nb::(generate_rna other)
		end

let rec print_rna r = match r with
	| [] -> print_char '\n'
	| head::other ->
		begin
		print_string (nb_to_str head);
		print_rna other
		end

let () =
	Random.self_init ();
	let n = read_int () in
	let he = generate_helix n in
	let che = complementary_helix he in
	begin
		print_endline (helix_to_string he);
		print_endline (helix_to_string che);
		let r = generate_rna he in
			print_rna r
	end
