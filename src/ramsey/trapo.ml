open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsautohelper;;
open Tcsset;;
open Tcslist;;
open Tcstiming;;



module MetaTrapo = struct

	type ('q, 'a, 'x, 'y) t = {
		alphabet: 'a Alphabet.alphabet;
		alphabet_iter: 'a Iterators.iterator;
		states: 'q Domain.t;
		state_iterator: 'q Iterators.iterator;
		initial: 'q;
		source: 'x Domain.t;
		source_iterator: 'x Iterators.iterator;
		state_source: ('q * 'x) Domain.t;
		state_source_iterator: ('q * 'x) Iterators.iterator;
		target: 'y Domain.t;
		target_iterator: 'y Iterators.iterator;
		state_target: ('q * 'y) Domain.t;
		state_target_iterator: ('q * 'y) Iterators.iterator;
		omega: 'q -> int;
		omega_combine: int -> int -> int;
		delta: ('q * 'x) -> 'a -> ('q * 'y) Iterators.iterator;
	}
	
	let alphabet meta = meta.alphabet
	
	let alphabet_iter meta = meta.alphabet_iter
	
	let states meta = meta.states
	
	let state_iterator meta = meta.state_iterator
	
	let initial meta = meta.initial
	
	let source meta = meta.source
	
	let source_iterator meta = meta.source_iterator

	let state_source meta = meta.state_source

	let state_source_iterator meta = meta.state_source_iterator

	let target meta = meta.target
	
	let target_iterator meta = meta.target_iterator

	let state_target meta = meta.state_target

	let state_target_iterator meta = meta.state_target_iterator
	
	let omega meta = meta.omega
	
	let omega_combine meta = meta.omega_combine
	
	let delta meta = meta.delta
	
	let compose meta_left meta_right delta_combined = {
		alphabet = meta_left.alphabet;
		alphabet_iter = meta_left.alphabet_iter;
		states = meta_left.states;
		state_iterator = meta_left.state_iterator;
		initial = meta_left.initial;
		source = meta_left.source;
		source_iterator = meta_left.source_iterator;
		state_source = meta_left.state_source;
		state_source_iterator = meta_left.state_source_iterator;
		target = meta_right.target;
		target_iterator = meta_right.target_iterator;
		state_target = meta_right.state_target;
		state_target_iterator = meta_right.state_target_iterator;
		omega = meta_left.omega;
		omega_combine = meta_left.omega_combine;
		delta = delta_combined;
	}
	
	let make alphabet states initial omega omega_combine delta state_iter alphabet_iter source source_iter target target_iter = {
		alphabet = alphabet;
		alphabet_iter = alphabet_iter;
		states = states;
		state_iterator = state_iter;
		initial = initial;
		source = source;
		source_iterator = source_iter;
		state_source = Domain.product states source;
		state_source_iterator = Iterators.product state_iter source_iter;
		target = target;
		target_iterator = target_iter;
		state_target = Domain.product states target;
		state_target_iterator = Iterators.product state_iter target_iter;
		omega = omega;
		omega_combine = omega_combine;
		delta = delta;
	}
	
end;;



module Trapo = struct

	type ('q, 'a, 'x, 'y) t = (('q, ('x, (('q * 'y), int) TreeMap.t) TreeMap.t) TreeMap.t * ('a Alphabet.word))
	
	
	let format_transition meta q x q' y i =
		"(" ^ Domain.format (MetaTrapo.states meta) q ^ "," ^ Domain.format (MetaTrapo.source meta) x ^ ")" ^
		"-" ^ string_of_int i ^ "->" ^
		"(" ^ Domain.format (MetaTrapo.states meta) q' ^ "," ^ Domain.format (MetaTrapo.target meta) y ^ ")"
	
	
	let format_transitions meta (trapo, _) =
		ListUtils.format (fun (q, x, q', y, i) -> format_transition meta q x q' y i)
		                 (TreeMap.fold (fun q -> TreeMap.fold (fun x -> TreeMap.fold (fun (q', y) i l -> (q, x, q', y, i)::l))) trapo [])
	
	
	let format_word meta (_, w) =
		Alphabet.format_word (MetaTrapo.alphabet meta) w
	
	
	let format meta trapo =
		"(" ^ (format_transitions meta trapo) ^ ", " ^ (format_word meta trapo) ^ ")"


	let get_word = snd
	
	let set_word (t, _) w = (t, w)
	

	let compare_words meta (_, w) (_, w') = Alphabet.compare_words (MetaTrapo.alphabet meta) w w'
	
	
	let compare_transitions meta (t, _) (t', _) = TreeMap.compare (TreeMap.compare (TreeMap.compare compare)) t t'
	
	
	let compare_full meta = Comparators.inner_product (compare_transitions meta) (compare_words meta)


	let compare = compare_transitions


	let single meta a =
		let t = Iterators.fold (MetaTrapo.state_iterator meta) (fun q t ->
			let p = MetaTrapo.omega meta q in
			let q_image = Iterators.fold (MetaTrapo.source_iterator meta) (fun x t ->
				let delta_image = Iterators.fold (MetaTrapo.delta meta (q, x) a) (fun (q', y) ->
					TreeMap.add (q', y) (MetaTrapo.omega_combine meta p (MetaTrapo.omega meta q'))
				) (TreeMap.empty (Domain.compare (MetaTrapo.state_target meta))) in
				if TreeMap.is_empty delta_image then t else TreeMap.add x delta_image t
			) (TreeMap.empty (Domain.compare (MetaTrapo.source meta))) in
			if TreeMap.is_empty q_image then t else TreeMap.add q q_image t
		) (TreeMap.empty (Domain.compare (MetaTrapo.states meta))) in
		let w = Alphabet.singleton_word a in
		(t, w)


(* To Check: f is idempotent iff for all q,q' with f(q,q') defined, we have: f(q,q') = max(prio. order) { f(q,q'), f(q',q') } *)		
						
	let is_idempotent meta (t, _) =
		let same_prio_result p (q', y) image =
			let p' = TreeMap.fold (fun (q'', y') p' acc ->
				try
					let p'' = MetaTrapo.omega_combine meta p' (TreeMap.find (q', y) (TreeMap.find y' (TreeMap.find q'' t))) in
					if acc = -1 then p'' else Priorities.reward_max p'' acc
				with
					Not_found -> acc
			) image (-1)
			in
				p = p'
		in
		let same_reachability_result (q', y) image =
			try
				TreeMap.for_all (fun (q'', z) _ -> TreeMap.mem (q'', z) image) (TreeMap.find y (TreeMap.find q' t))
			with
				Not_found -> true
		in
		TreeMap.for_all (fun q ->
			TreeMap.for_all (fun x image ->
				TreeMap.for_all (fun (q', y) p ->
					same_prio_result p (q', y) image && same_reachability_result (q', y) image
				) image
			)
		) t


	let image meta (t, _) state_source_iterator =
		Iterators.fold state_source_iterator (fun (q, x) acc ->
			try
				TreeMap.fold (fun (q', y) _ -> TreeSet.add (q', y)) (TreeMap.find x (TreeMap.find q t)) acc
			with
				Not_found -> acc
		) (TreeSet.empty (Domain.compare (MetaTrapo.state_target meta)))
	

	let preimage meta (t, _) state_target_set =
		TreeMap.fold (fun q ->
			TreeMap.fold (fun x image acc ->
				if TreeMap.fold (fun (q', y) _ b -> b || TreeSet.mem (q', y) state_target_set) image false
				then TreeSet.add (q, x) acc
				else acc
			)
		) t (TreeSet.empty (Domain.compare (MetaTrapo.state_source meta)))
		
		
		
	let compose meta_left meta_right map (trans_left, word_left) (trans_right, word_right) =
		let t = TreeMap.fold (fun q annot t ->
			let res = TreeMap.fold (fun x image_left t ->
				let cod = TreeMap.fold (fun (q', y) p cod ->
					match TreeMap.find_opt q' trans_right with
						Some h -> (
							match TreeMap.find_opt (map y) h with
								Some img -> 
									TreeMap.fold (fun (q'', z) p' cod ->
										let p'' = MetaTrapo.omega_combine meta_left p p' in
										match TreeMap.find_opt (q'', z) cod with
											Some p''' -> if Priorities.reward_max p'' p''' != p''' then TreeMap.add (q'', z) p'' cod else cod
										|	None -> TreeMap.add (q'', z) p'' cod
									) img cod
							|	None -> cod
						)
					|	None -> cod
				) image_left (TreeMap.empty (Domain.compare (MetaTrapo.state_target meta_right))) in
				if TreeMap.is_empty cod then t else TreeMap.add x cod t
			) annot (TreeMap.empty (Domain.compare (MetaTrapo.source meta_left))) in
			if TreeMap.is_empty res then t else TreeMap.add q res t
		) trans_left (TreeMap.empty (Domain.compare (MetaTrapo.states meta_left))) in
		let w = Alphabet.compose_words word_left word_right in
		(t, w)

	let compose_right meta_left meta_right (trans_left, word_left) (trans_right, word_right) =
		let t = TreeMap.fold (fun q annot t ->
			let res = TreeMap.fold (fun x image_left t ->
				let cod = TreeMap.fold (fun (q', y) p cod ->
					try
						TreeMap.fold (fun (q'', ()) p' cod ->
							let p'' = MetaTrapo.omega_combine meta_left p p' in
							try
								let p''' = TreeMap.find (q'', y) cod in
								if Priorities.reward_max p'' p''' != p''' then TreeMap.add (q'', y) p'' cod else cod
							with
								Not_found -> TreeMap.add (q'', y) p'' cod
						) (TreeMap.find () (TreeMap.find q' trans_right)) cod
					with
						Not_found -> cod
				) image_left (TreeMap.empty (Domain.compare (MetaTrapo.state_target meta_left))) in
				if TreeMap.is_empty cod then t else TreeMap.add x cod t
			) annot (TreeMap.empty (Domain.compare (MetaTrapo.source meta_left))) in
			if TreeMap.is_empty res then t else TreeMap.add q res t
		) trans_left (TreeMap.empty (Domain.compare (MetaTrapo.states meta_left))) in
		let w = Alphabet.compose_words word_left word_right in
		(t, w)
	
	let compose_left meta_left meta_right (trans_left, word_left) (trans_right, word_right) =
		let cmp_states = (Domain.compare (MetaTrapo.states meta_right)) in
		let cmp_source = (Domain.compare (MetaTrapo.source meta_right)) in
		let cmp_tarsta = (Domain.compare (MetaTrapo.state_target meta_right)) in
		let l = TreeMap.fold (fun q ->
			TreeMap.fold (fun () ->
				TreeMap.fold (fun (q',()) p l ->
					try
						TreeMap.fold (fun x ->
							TreeMap.fold (fun (q'', y) p' l ->
								(q, q'', x, y, MetaTrapo.omega_combine meta_right p p')::l
							)
						) (TreeMap.find q' trans_right) l
					with
						Not_found -> l
				)
			)
		) trans_left [] in
		let t = List.fold_left (fun acc (q, q', x, y, p) ->
			try
				let qsub = TreeMap.find q acc in
				try
					let xsub = TreeMap.find x qsub in
					try
						let p' = TreeMap.find (q', y) xsub in
						if Priorities.reward_max p' p != p'
						then TreeMap.add q (TreeMap.add x (TreeMap.add (q', y) p xsub) qsub) acc
						else acc
					with
						Not_found -> TreeMap.add q (TreeMap.add x (TreeMap.add (q', y) p xsub) qsub) acc
				with
					Not_found -> TreeMap.add q (TreeMap.add x (TreeMap.singleton cmp_tarsta (q', y) p) qsub) acc
			with
				Not_found -> TreeMap.add q (TreeMap.singleton cmp_source x (TreeMap.singleton cmp_tarsta (q', y) p)) acc
		) (TreeMap.empty cmp_states) l in
		let w = Alphabet.compose_words word_left word_right in
		(t, w)
		
		let collapse_right meta meta_collapsed (trapo, word) =
			let trapo' = TreeMap.map (TreeMap.map (fun map -> TreeMap.fold (fun (q, _) i acc ->
				match TreeMap.find_opt (q, ()) acc with
				| None -> TreeMap.add (q, ()) i acc
				| Some j -> if Priorities.reward_max j i == i then TreeMap.add (q, ()) i acc else acc
			) map (TreeMap.empty (Domain.compare (MetaTrapo.state_target meta_collapsed))))) trapo
			in
			  (trapo', word)
				
		let project_left meta (trapo, word) symbol =
			let trapo' = TreeMap.fold (fun q rest acc ->
				match TreeMap.find_opt symbol rest with
				| None -> acc
				| Some inner -> TreeMap.add q (TreeMap.singleton_def () inner) acc
		  ) trapo (TreeMap.empty (Domain.compare (MetaTrapo.states meta)))
			in
			  (trapo', word)
				
end;;


module TrapoSet = struct

	type ('q, 'a, 'x, 'y) t = (('q, 'a, 'x, 'y) Trapo.t) TreeSet.t
	
	let format meta = TreeSet.format (fun m -> Trapo.format meta m ^ "\n")
	
	let empty meta = TreeSet.empty (Trapo.compare meta)
	
	let mem = TreeSet.mem
	
	let is_empty = TreeSet.is_empty
	
	let add = TreeSet.add
	
	let singles meta = Iterators.fold (MetaTrapo.alphabet_iter meta) (fun a -> add (Trapo.single meta a)) (empty meta)

	let fold = TreeSet.fold
	
	let iter = TreeSet.iter

	let union = TreeSet.union
	
	let diff = TreeSet.diff
	
	let cardinal = TreeSet.cardinal
	
	let join left_meta right_meta composed_meta composer left_set right_set =
		fold (fun m -> fold (fun m' -> add (composer m m')) right_set) left_set (empty composed_meta)
		
	let conditional_join left_meta right_meta composed_meta composer left_set right_set add_filter abort_condition =
		let do_abort = ref false in
		fold (fun m t ->
			if !do_abort then t
			else (
				fold (fun m' t ->
					if !do_abort then t
					else (
						let m'' = composer m m' in
						if add_filter m'' then (
							do_abort := abort_condition m'';
							add m'' t
						)
						else t
					)
				) right_set t
			)
		) left_set (empty composed_meta)
	
end;;



module StatesetSet = struct

	type ('q, 'a) t = ('q, 'a Alphabet.word) SubsetSet.t
	
	let empty meta = SubsetSet.empty (Domain.compare (MetaTrapo.states meta))
	
	let epsilon meta = 
		let cmp = Domain.compare (MetaTrapo.states meta) in
		SubsetSet.singleton cmp (TreeSet.singleton cmp (MetaTrapo.initial meta), Alphabet.empty_word ())

	let subsumed reachers set =
		not (TreeSet.is_empty (SubsetSet.subsets reachers set))

	let add reachers set word =
		SubsetSet.add_subsume_supersets reachers (set, word)
	
	let union reachers reachersx = 
		TreeSet.fold (fun reacher_id acc ->
			let (reacher, word) = SubsetSet.lookup reachersx reacher_id in
			SubsetSet.add_subsume_supersets acc (reacher, word)
		) (SubsetSet.get_sets reachersx) reachers
	
	let reacher_idempot_match meta reachers idempots =
		let found = ref None in
		TreeSet.iter (fun reacher_id ->
			if !found = None then (
				let (reacher, reacher_word) = SubsetSet.lookup reachers reacher_id in
				let disj = SubsetSet.disjointsets idempots reacher in
				if not (TreeSet.is_empty disj) then (
					let idempot_id = TreeSet.min_elt disj in
					let (_, idempot_word) = SubsetSet.lookup idempots idempot_id in
					found := Some (Alphabet.format_omega_word (MetaTrapo.alphabet meta) (reacher_word, Alphabet.omega_simplify (MetaTrapo.alphabet meta) idempot_word))
				)
			)
		) (SubsetSet.get_sets reachers);
		!found

end;;



module SimpleMetaTrapo = struct

	type ('q, 'a) t = ('q, 'a, unit, unit) MetaTrapo.t

	let make alphabet states initial omega omega_combine delta state_iter alphabet_iter = 
		MetaTrapo.make alphabet states initial omega omega_combine (fun (q, _) a -> Iterators.product (delta q a) (Iterators.singleton ()))
		               state_iter alphabet_iter (Domain.make compare (fun _ -> "()")) (Iterators.singleton ())
		               (Domain.make compare (fun _ -> "()")) (Iterators.singleton ())
		               
end;;


module LeftMetaTrapo = struct

	type ('q, 'a, 'x) t = ('q, 'a, 'x, unit) MetaTrapo.t

	let make alphabet states initial omega omega_combine delta state_iter alphabet_iter source source_iter = 
		MetaTrapo.make alphabet states initial omega omega_combine (fun (q, x) a -> Iterators.product (delta q a x) (Iterators.singleton ()))
		               state_iter alphabet_iter source source_iter
		               (Domain.make compare (fun _ -> "()")) (Iterators.singleton ())
		               
end;;


module RightMetaTrapo = struct

	type ('q, 'a, 'y) t = ('q, 'a, unit, 'y) MetaTrapo.t

	let make alphabet states initial omega omega_combine delta state_iter alphabet_iter target target_iter = 
		MetaTrapo.make alphabet states initial omega omega_combine (fun (q, _) a -> delta q a)
		               state_iter alphabet_iter (Domain.make compare (fun _ -> "()")) (Iterators.singleton ())
		               target target_iter
		               
end;;


module SimpleTrapo = struct

	type ('q, 'a) t = ('q, 'a, unit, unit) Trapo.t
	
	let format_transitions = Trapo.format_transitions
	
	let format_word = Trapo.format_word
	
	let format = Trapo.format
	
	let get_word = Trapo.get_word

	let compare_words = Trapo.compare_words
	
	let compare_transitions = Trapo.compare_transitions
	
	let compare_full = Trapo.compare_full

	let compare = Trapo.compare

	let single = Trapo.single
	
	let compose meta a b = Trapo.compose meta meta (fun x -> x) a b

	let is_idempotent = Trapo.is_idempotent

	let image meta x it = TreeSet.map2 (Domain.compare (MetaTrapo.states meta)) fst (Trapo.image meta x (Iterators.product it (Iterators.singleton ())))

	let preimage meta x s = TreeSet.map2 (Domain.compare (MetaTrapo.states meta)) fst (Trapo.preimage meta x (TreeSet.map2 (Domain.compare (MetaTrapo.state_source meta)) (fun x -> (x, ())) s))

	let epsilon meta =
		let t = Iterators.fold (MetaTrapo.state_source_iterator meta) (fun (q, ()) ->
			TreeMap.add q (
				TreeMap.add ()
				(TreeMap.add (q, ()) (MetaTrapo.omega meta q) (TreeMap.empty (Domain.compare (MetaTrapo.state_target meta))))
				(TreeMap.empty (Domain.compare (MetaTrapo.source meta)))
			)
		) (TreeMap.empty (Domain.compare (MetaTrapo.states meta))) in
		(t, Alphabet.empty_word ()) 

	let good_states meta (t, _) =
		TreeMap.fold (fun q ->
			TreeMap.fold (fun () image acc ->
				try
					if (TreeMap.find (q, ()) image) mod 2 = 0
					then TreeSet.add q acc
					else acc
				with
					Not_found -> acc
			)
		) t (TreeSet.empty (Domain.compare (MetaTrapo.states meta)))

end;;


module NpaMetaTrapo = struct

	type ('q, 'a) t = ('q, 'a) SimpleMetaTrapo.t

	let make npa state_iter alphabet_iter =
		SimpleMetaTrapo.make (NMA.alphabet npa) (NMA.states npa) (NMA.initial npa) (NPA.omega npa) Priorities.relevance_max (NMA.delta npa) state_iter alphabet_iter

end;;



module FiniteNbaMetaTrapo = struct

	type ('q, 'a) t = ('q, 'a) SimpleMetaTrapo.t

	let make nba state_iter alphabet_iter =
		SimpleMetaTrapo.make (NMA.alphabet nba) (NMA.states nba) (NMA.initial nba) (NPA.omega (NBA.asNPA nba)) (fun left right -> right) (NMA.delta nba) state_iter alphabet_iter

end;;



module NpvpaMetaTrapo = struct

	type ('q, 'a) i = ('q, ('a NMVPA.nested)) SimpleMetaTrapo.t
	type ('q, 'a, 's) r = ('q, ('a NMVPA.nested), 's option, unit) MetaTrapo.t
	type ('q, 'a, 's) c = ('q, ('a NMVPA.nested), unit, 's) MetaTrapo.t
	
	let make_i npvpa state_iter alphabet_iter =
		SimpleMetaTrapo.make (NMVPA.alphabet npvpa) (NMVPA.states npvpa) (NMVPA.initial npvpa) (NPVPA.omega npvpa) Priorities.relevance_max (NMVPA.delta_internal npvpa) state_iter (Iterators.filter (function NMVPA.Internal _ -> true | _ -> false) alphabet_iter)
	
	let make_r npvpa state_iter alphabet_iter stack_iter =
		LeftMetaTrapo.make (NMVPA.alphabet npvpa) (NMVPA.states npvpa) (NMVPA.initial npvpa) (NPVPA.omega npvpa) Priorities.relevance_max (NMVPA.delta_pop npvpa) state_iter (Iterators.filter (function NMVPA.Pop _ -> true | _ -> false) alphabet_iter) (Domain.option_domain (NMVPA.stack npvpa)) (Iterators.option_iterator stack_iter)
	
	let make_c npvpa state_iter alphabet_iter stack_iter =
		RightMetaTrapo.make (NMVPA.alphabet npvpa) (NMVPA.states npvpa) (NMVPA.initial npvpa) (NPVPA.omega npvpa) Priorities.relevance_max (NMVPA.delta_push npvpa) state_iter (Iterators.filter (function NMVPA.Push _ -> true | _ -> false) alphabet_iter) (NMVPA.stack npvpa) stack_iter
		
	let make npvpa state_iter alphabet_iter stack_iter =
		(make_i npvpa state_iter alphabet_iter, make_r npvpa state_iter alphabet_iter stack_iter, make_c npvpa state_iter alphabet_iter stack_iter)

end




module FiniteNbvpaMetaTrapo = struct

	type ('q, 'a) i = ('q, ('a NMVPA.nested)) SimpleMetaTrapo.t
	type ('q, 'a, 's) r = ('q, ('a NMVPA.nested), 's option, unit) MetaTrapo.t
	type ('q, 'a, 's) c = ('q, ('a NMVPA.nested), unit, 's) MetaTrapo.t

	let make_i nbvpa state_iter alphabet_iter =
		SimpleMetaTrapo.make (NMVPA.alphabet nbvpa) (NMVPA.states nbvpa) (NMVPA.initial nbvpa) (NPVPA.omega (NBVPA.asNPVPA nbvpa)) (fun left right -> right) (NMVPA.delta_internal nbvpa) state_iter (Iterators.filter (function NMVPA.Internal _ -> true | _ -> false) alphabet_iter)
	
	let make_r nbvpa state_iter alphabet_iter stack_iter =
		LeftMetaTrapo.make (NMVPA.alphabet nbvpa) (NMVPA.states nbvpa) (NMVPA.initial nbvpa) (NPVPA.omega (NBVPA.asNPVPA nbvpa)) (fun left right -> right) (NMVPA.delta_pop nbvpa) state_iter (Iterators.filter (function NMVPA.Pop _ -> true | _ -> false) alphabet_iter) (Domain.option_domain (NMVPA.stack nbvpa)) (Iterators.option_iterator stack_iter)
	
	let make_c nbvpa state_iter alphabet_iter stack_iter =
		RightMetaTrapo.make (NMVPA.alphabet nbvpa) (NMVPA.states nbvpa) (NMVPA.initial nbvpa) (NPVPA.omega (NBVPA.asNPVPA nbvpa)) (fun left right -> right) (NMVPA.delta_push nbvpa) state_iter (Iterators.filter (function NMVPA.Push _ -> true | _ -> false) alphabet_iter) (NMVPA.stack nbvpa) stack_iter
		
	let make nbvpa state_iter alphabet_iter stack_iter =
		(make_i nbvpa state_iter alphabet_iter, make_r nbvpa state_iter alphabet_iter stack_iter, make_c nbvpa state_iter alphabet_iter stack_iter)

end
