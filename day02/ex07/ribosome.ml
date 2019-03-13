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

let rec generate_bases_triplets r = match r with
	| first::second::third::other ->
		let triple = (first, second, third) in
			triple::(generate_bases_triplets other)
	| _ -> []

type aminoacid = Stop|Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|Tyr|Val|None
type protein = aminoacid list

let rec string_of_protein po = match po with
	| [] -> ""
	| head::other ->
		let s = match head with
			|Stop -> "UAAUAGUGA"
			|Ala -> "GCAGCCGCGGCU"
			|Arg -> "AGAAGGCGACGCCGGCGU"
			|Asn -> "AACAAU"
			|Asp -> "GACGAU"
			|Cys -> "UGCUGU"
			|Gln -> "CAACAG"
			|Glu -> "GAAGAG"
			|Gly -> "GGAGGCGGGGGU"
			|His -> "CACCAU"
			|Ile -> "AUAAUCAUU"
			|Leu -> "CUACUCCUGCUUUUAUUG"
			|Lys -> "AAAAAG"
			|Met -> "AUG"
			|Phe -> "UUCUUU"
			|Pro -> "CCCCCACCGCCU"
			|Ser -> "UCAUCCUCGUCU"
			|Thr -> "ACAACCACGACU"
			|Trp -> "UGG"
			|Tyr -> "UACUAU"
			|Val -> "GUAGUCGUGGUU"
			|None -> "." in
			s ^ (string_of_protein other)


let decode_arn r : protein =
	let triplets = generate_bases_triplets r in
	let rec tri_to_protein t = match t with
		| (U,A,A)::(U,A,G)::(U,G,A)::other -> Stop::[]
		| (G,C,A)::(G,C,C)::(G,C,G)::(G,C,U)::other ->
			Ala::(tri_to_protein other)
		| (A,G,A)::(A,G,G)::(C,G,A)::(C,G,C)::(C,G,G)::(C,G,U)::other ->
			Arg::(tri_to_protein other)
		| (A,A,C)::(A,A,U)::other ->
			Asn::(tri_to_protein other)
		| (G,A,C)::(G,A,U)::other ->
			Asp::(tri_to_protein other)
		| (U,G,C)::(U,G,U)::other ->
			Cys::(tri_to_protein other)
		| (C,A,A)::(C,A,G)::other ->
			Gln::(tri_to_protein other)
		| (G,A,A)::(G,A,G)::other ->
			Glu::(tri_to_protein other)
		| (G,G,A)::(G,G,C)::(G,G,G)::(G,G,U)::other ->
			Gly::(tri_to_protein other)
		| (C,A,C)::(C,A,U)::other ->
			His::(tri_to_protein other)
		| (A,U,A)::(A,U,C)::(A,U,U)::other ->
			Ile::(tri_to_protein other)
		| (C,U,A)::(C,U,C)::(C,U,G)::(C,U,U)::(U,U,A)::(U,U,G)::other ->
			Leu::(tri_to_protein other)
		| (A,A,A)::(A,A,G)::other ->
			Lys::(tri_to_protein other)
		| (A,U,G)::other ->
			Met::(tri_to_protein other)
		| (U,U,C)::(U,U,U)::other ->
			Phe::(tri_to_protein other)
		| (C,C,C)::(C,C,A)::(C,C,G)::(C,C,U)::other ->
			Pro::(tri_to_protein other)
		| (U,C,A)::(U,C,C)::(U,C,G)::(U,C,U)::other ->
			Ser::(tri_to_protein other)
		| (A,C,A)::(A,C,C)::(A,C,G)::(A,C,U)::other ->
			Thr::(tri_to_protein other)
		| (U,G,G)::other ->
			Trp::(tri_to_protein other)
		| (U,A,C)::(U,A,U)::other ->
			Tyr::(tri_to_protein other)
		| (G,U,A)::(G,U,C)::(G,U,G)::(G,U,U)::other ->
			Val::(tri_to_protein other)
		| _ -> None::[] in
		tri_to_protein triplets

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

