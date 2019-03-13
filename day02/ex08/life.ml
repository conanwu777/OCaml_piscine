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

type aminoacid = Stop|Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met
|Phe|Pro|Ser|Thr|Trp|Tyr|Val|None
type protein = aminoacid list

let rec string_of_protein po = match po with
	| [] -> ""
	| head::other ->
		let s = match head with
			|Stop -> "End"
			|Ala -> "Alanine"
			|Arg -> "Arginine"
			|Asn -> "Asparagine"
			|Asp -> "Aspartique"
			|Cys -> "Cysteine"
			|Gln -> "Glutamine"
			|Glu -> "Glutamique"
			|Gly -> "Glycine"
			|His -> "Histidine"
			|Ile -> "Isoleucine"
			|Leu -> "Leucine"
			|Lys -> "Lysine"
			|Met -> "Methionine"
			|Phe -> "Phenylalanine"
			|Pro -> "Proline"
			|Ser -> "Serine"
			|Thr -> "Threonine"
			|Trp -> "Tryptophane"
			|Tyr -> "Tyrosine"
			|Val -> "Valine"
			|None -> "_N/A_" in
			s ^ (string_of_protein other)

let decode_arn r : protein =
	let triplets = generate_bases_triplets r in
	let rec tri_to_protein t = match t with
		| head::other ->
			let first = match head with
				| (U,A,A) | (U,A,G) | (U,G,A) -> Stop
				| (G,C,A) | (G,C,C) | (G,C,G) | (G,C,U) -> Ala
				| (A,G,A) | (A,G,G) | (C,G,A) | (C,G,C) | (C,G,G) | (C,G,U) -> Arg
				| (A,A,C) | (A,A,U) -> Asn
				| (G,A,C) | (G,A,U) -> Asp
				| (U,G,C) | (U,G,U) -> Cys
				| (C,A,A) | (C,A,G) -> Gln
				| (G,A,A) | (G,A,G) -> Glu
				| (G,G,A) | (G,G,C) | (G,G,G) | (G,G,U) -> Gly
				| (C,A,C) | (C,A,U) -> His
				| (A,U,A) | (A,U,C) | (A,U,U) -> Ile
				| (C,U,A) | (C,U,C) | (C,U,G) | (C,U,U) | (U,U,A) | (U,U,G) -> Leu
				| (A,A,A) | (A,A,G) -> Lys
				| (A,U,G) -> Met
				| (U,U,C) | (U,U,U) -> Phe
				| (C,C,C) | (C,C,A) | (C,C,G) | (C,C,U) -> Pro
				| (U,C,A) | (U,C,C) | (U,C,G) | (U,C,U) -> Ser
				| (A,C,A) | (A,C,C) | (A,C,G) | (A,C,U) -> Thr
				| (U,G,G) -> Trp
				| (U,A,C) | (U,A,U) -> Tyr
				| (G,U,A) | (G,U,C) | (G,U,G) | (G,U,U) -> Val
				| _ -> None in
				if first == Stop then first::[]
				else first::(tri_to_protein other)
		| [] -> [] in
	tri_to_protein triplets

let rec print_triples triplets = match triplets with
	| [] -> print_char '\n'
	| head::other ->
		let print_triple head = match head with (a, b, c) ->
			begin
				print_string (nb_to_str a);
				print_string (nb_to_str b);
				print_string (nb_to_str c)
			end in
			print_triple head;
		let e other = match other with
			| [] -> ()
			| _ -> print_char ' ' in
			e other;
		print_triples other

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
	begin
		print_endline (helix_to_string he);
		let r = generate_rna he in
		begin
			print_rna r;
			print_triples (generate_bases_triplets r);
			let po = decode_arn r in
				print_endline (string_of_protein po)
		end
	end

