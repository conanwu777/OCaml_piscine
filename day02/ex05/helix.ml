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
			| _ -> 'N' in
		(generate_nucleotide c)::(generate_helix (n - 1))	

let rec helix_to_string he = match he with
	| [] -> ""
	| head::other ->
		begin
		let c = match head.nucleobase with
			| A -> "A"
			| T -> "T"
			| C -> "C"
			| G -> "G"
			| None -> "N" in
		c ^ (helix_to_string other)
		end

let rec complementary_helix he : helix = match he with
	| [] -> []
	| head::other ->
		let c = match head.nucleobase with
			| A -> 'T'
			| T -> 'A'
			| C -> 'G'
			| G -> 'C'
			| None -> 'N'
		in
			(generate_nucleotide c)::(complementary_helix other)

let () =
	Random.self_init ();
	let n = read_int () in
	let he = generate_helix n in
	let che = complementary_helix he in
	begin
		print_endline (helix_to_string he);
		print_endline (helix_to_string che)
	end
