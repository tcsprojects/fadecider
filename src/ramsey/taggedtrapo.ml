open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsautohelper;;
open Tcsset;;
open Tcslist;;
open Tcstiming;;
open Solverregistry;;
open Trapo;;


module MetaTaggedTrapo = struct

	type ('q, 'a, 'x, 'y, 'p, 'r, 's) t = ('q, 'a, 'x, 'y) MetaTrapo.t * ('p, 'a, 'r, 's) MetaTrapo.t
	
	let make x y = (x, y)
	
	let left = fst
	
	let right = snd

end;;



module TaggedTrapo = struct

	type ('q, 'a, 'x, 'y, 'p, 'r, 's) t = ('q, 'a, 'x, 'y) Trapo.t * ('p * 'r * 'p * 's * int)
		
	let format_tagged_transition meta' (_, (q, x, q', y, i)) =
		let meta = MetaTaggedTrapo.right meta' in
		"(" ^ Domain.format (MetaTrapo.states meta) q ^ "," ^ Domain.format (MetaTrapo.source meta) x ^ ")" ^
		"-" ^ string_of_int i ^ "->" ^
		"(" ^ Domain.format (MetaTrapo.states meta) q' ^ "," ^ Domain.format (MetaTrapo.target meta) y ^ ")"
	
	
	let format_transitions meta (l,_) = Trapo.format_transitions (MetaTaggedTrapo.left meta) l
	
	let format_word meta (l,_) = Trapo.format_word (MetaTaggedTrapo.left meta) l
	
	let format meta tt = "(" ^ format_transitions meta tt ^ ", " ^ format_tagged_transition meta tt ^ ", " ^ format_word meta tt ^ ")"

	let get_word (l,_) = Trapo.get_word l
	
	let get_trapo = fst
	
	let make x y = (x,y)

	let compare_words meta (l, _) (l', _) = Trapo.compare_words (MetaTaggedTrapo.left meta) l l'
	
	let compare_transitions meta (l, _) (l', _) = Trapo.compare_transitions (MetaTaggedTrapo.left meta) l l'
	
	let compare_tagged_transitions meta r r' =
		let m = MetaTaggedTrapo.right meta in
		Comparators.product5 (Domain.compare (MetaTrapo.states m)) (Domain.compare (MetaTrapo.source m)) (Domain.compare (MetaTrapo.states m)) (Domain.compare (MetaTrapo.target m)) compare r r'
		
	let compare_all_transitions meta = Comparators.inner_product (compare_transitions meta) (fun x y -> compare_tagged_transitions meta (snd x) (snd y))
	
	let compare_full meta = Comparators.inner_product (compare_all_transitions meta) (compare_words meta)

	let compare = compare_all_transitions

	let single meta a p r p' s =
		let m = MetaTaggedTrapo.right meta in
		(Trapo.single (MetaTaggedTrapo.left meta) a,
		 (p, r, p', s, (MetaTrapo.omega_combine m (MetaTrapo.omega m p) (MetaTrapo.omega m p'))))
		 
	let decoration (_, (p, r, p', s, i)) = (p,r,i,p',s)

	let composable meta (_, (_, _, q, _, _)) (_, (q', _, _, _, _)) =
		(Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta))) q q' = 0

	let is_idempotent meta ((trapo, deco) as tt) =
		composable meta tt tt && Trapo.is_idempotent (MetaTaggedTrapo.left meta) trapo

	let image meta (trapo, _) =
		Trapo.image (MetaTaggedTrapo.left meta) trapo
		
	let preimage meta (trapo, _) =
		Trapo.preimage (MetaTaggedTrapo.left meta) trapo

	let compose meta_left meta_right map_inner (trapo_left, (q,r,_,_,p)) (trapo_right, (_,_,q',s,p')) =
		(Trapo.compose (MetaTaggedTrapo.left meta_left) (MetaTaggedTrapo.left meta_right) map_inner trapo_left trapo_right,
		 (q,r,q',s, MetaTrapo.omega_combine (MetaTaggedTrapo.right meta_left) p p'))
		
	let compose_left meta_left meta_right (trapo_left, (q,_,_,_,p)) (trapo_right, (_,r,q',s,p')) =
		(Trapo.compose_left (MetaTaggedTrapo.left meta_left) (MetaTaggedTrapo.left meta_right) trapo_left trapo_right,
		 (q,r,q',s, MetaTrapo.omega_combine (MetaTaggedTrapo.right meta_left) p p'))
		
	let compose_right meta_left meta_right (trapo_left, (q,r,_,s,p)) (trapo_right, (_,_,q',_,p')) =
		(Trapo.compose_right (MetaTaggedTrapo.left meta_left) (MetaTaggedTrapo.left meta_right) trapo_left trapo_right,
		 (q,r,q',s, MetaTrapo.omega_combine (MetaTaggedTrapo.right meta_left) p p'))
		
	let collapse_right meta meta_collapsed (trapo, (q,r,q',_,p)) =
		(Trapo.collapse_right (MetaTaggedTrapo.left meta) (MetaTaggedTrapo.left meta_collapsed) trapo,
		 (q,r,q',(),p))
		
	let project_left meta (trapo, (q,symbol',q',s,p)) symbol symbol'' =
		if symbol'' = symbol' then Some
			(Trapo.project_left (MetaTaggedTrapo.left meta) trapo symbol,
			 (q,(),q',s,p))
		else None

end;;



module TaggedTrapoSet = struct
	
	type ('q, 'a, 'x, 'y, 'p, 'r, 's) t = ('p * 'r, (('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t) TreeSet.t) TreeMap.t
	 
	let format meta = TreeMap.format (fun (_, b) -> TreeSet.format (TaggedTrapo.format meta) b)
	
	let empty meta = TreeMap.empty (Domain.compare (MetaTrapo.state_source (MetaTaggedTrapo.right meta)))

	let mem ((_, (q,r,_,_,_)) as tt) tts =
		try
			TreeSet.mem tt (TreeMap.find (q,r) tts)
		with
			Not_found -> false 
			
	
	let is_empty = TreeMap.is_empty
	
	let iter f = TreeMap.iter (fun _ -> TreeSet.iter f)
	
	let fold f = TreeMap.fold (fun _ -> TreeSet.fold f)

	let cardinal m =
		let i = ref 0 in
		TreeMap.iter (fun _ inner ->
			i := !i + TreeSet.cardinal inner
		) m;
		!i
	
	let add meta ((_, (q,r,_,_,_)) as tt) tts =
		let set = try
						TreeMap.find (q,r) tts
  				  with
						Not_found -> TreeSet.empty (TaggedTrapo.compare meta) 
		in
			TreeMap.add (q,r) (TreeSet.add tt set) tts
	
	let fold_sub f (q,r) tts acc =
		try
			TreeSet.fold f (TreeMap.find (q,r) tts) acc
		with
			Not_found -> acc
			
	let iter_sub f (q,r) tts =
		try
			TreeSet.iter f (TreeMap.find (q,r) tts)
		with
			Not_found -> ()
			
	let union tts tts' = 
		TreeMap.fold (fun (q,r) s m ->
			let set = try
						TreeSet.union (TreeMap.find (q,r) m) s
					  with
						Not_found -> s
			in
				TreeMap.add (q,r) set m
		) tts' tts
	
	let diff tts tts' = 
		TreeMap.fold (fun (q,r) s m ->
			try
				let s' = TreeMap.find (q,r) m in
				let s'' = TreeSet.diff s' s in
				if TreeSet.is_empty s''
				then TreeMap.remove (q,r) m
				else TreeMap.add (q,r) s'' m
			with
				Not_found -> m
		) tts' tts
			
	let singles meta =
		Iterators.fold (MetaTrapo.alphabet_iter (MetaTaggedTrapo.left meta)) (fun a ->
			Iterators.fold (MetaTrapo.state_source_iterator (MetaTaggedTrapo.right meta)) (fun (q,r) ->
				Iterators.fold (MetaTrapo.delta (MetaTaggedTrapo.right meta) (q,r) a) (fun (q',s) ->
					add meta (TaggedTrapo.single meta a q r q' s)
				)
			)
		) (empty meta)			
			
	let join composed_meta inner_join composer left_set right_set =
		fold (fun m acc ->
			let (_, _, _, p, s) = TaggedTrapo.decoration m in
			fold (fun m' acc ->
				let (pp, ss, _, _, _) = TaggedTrapo.decoration m' in
				if inner_join (p, s) (pp, ss)
				then add composed_meta (composer m m') acc
				else acc
			) right_set acc
		) left_set (empty composed_meta)
	
	let conditional_join_union left_meta right_meta composed_meta inner_join composer left_set right_set initial_set add_filter abort_condition =
		let do_abort = ref false in
		fold (fun m acc ->
			if !do_abort then acc else (
				let (_, _, _, p, s) = TaggedTrapo.decoration m in
				fold (fun m' acc ->
					if !do_abort then acc else (
						let (pp, ss, _, _, _) = TaggedTrapo.decoration m' in
						if inner_join (p, s) (pp, ss)
						then (
							let m'' = composer m m' in
							if (not (mem m'' acc)) && add_filter m'' then (
								do_abort := abort_condition m'';
								add composed_meta m'' acc
							)
							else acc
						)
						else acc
					)
				) right_set acc
			)
		) left_set initial_set



	let trapo_proj meta set = 
		fold (fun tt acc -> TrapoSet.add (TaggedTrapo.get_trapo tt) acc) set (TrapoSet.empty (MetaTaggedTrapo.left meta))
	
end;;



module TaggedStatesetSet = struct

	type ('q, 'a, 'p) t = ('p, ('q, 'a) StatesetSet.t) TreeMap.t
		
	let empty meta = TreeMap.empty (Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta)))
	
	let epsilon meta = 
		TreeMap.add (MetaTrapo.initial (MetaTaggedTrapo.right meta)) (StatesetSet.epsilon (MetaTaggedTrapo.left meta)) (empty meta)

	let subsumed reachers r set =
		try
			StatesetSet.subsumed (TreeMap.find r reachers) set
		with
			Not_found -> false

	let add meta reachers r set word =
		let states = try
		                 TreeMap.find r reachers
		             with
		             	Not_found -> StatesetSet.empty (MetaTaggedTrapo.left meta)
		in
			TreeMap.add r (StatesetSet.add states set word) reachers
	
	let union reachers reachersx = 
		TreeMap.fold (fun r statesetx acc ->
			try
				let stateset = TreeMap.find r acc in
				TreeMap.add r (StatesetSet.union stateset statesetx) acc
			with
				Not_found -> TreeMap.add r statesetx acc
		) reachersx reachers


	let reacher_idempot_match meta reachers idempots =
		TreeMap.fold (fun r s found ->
			if found = None then (
				try
					let i = TreeMap.find r idempots in
					StatesetSet.reacher_idempot_match (MetaTaggedTrapo.left meta) s i
				with
					Not_found -> None
			)
			else found
		) reachers None

end;;
	


module SimpleMetaTaggedTrapo = struct

	type ('q, 'a, 'p) t = ('q, 'a, unit, unit, 'p, unit, unit) MetaTaggedTrapo.t

	let make alphabet states initial omega omega_combine delta state_iter alphabet_iter
	         states' initial' omega' omega_combine' delta' state_iter' = 
		MetaTaggedTrapo.make (SimpleMetaTrapo.make alphabet states initial omega omega_combine delta state_iter alphabet_iter)
		                     (SimpleMetaTrapo.make alphabet states' initial' omega' omega_combine' delta' state_iter' alphabet_iter)
		               
end;;


module LeftMetaTaggedTrapo = struct

	type ('q, 'a, 'x, 'p, 's) t = ('q, 'a, 'x, unit, 'p, 's, unit) MetaTaggedTrapo.t

	let make alphabet states initial omega omega_combine delta state_iter alphabet_iter source source_iter 
	         states' initial' omega' omega_combine' delta' state_iter' source' source_iter' =
		MetaTaggedTrapo.make
			(MetaTrapo.make alphabet states initial omega omega_combine (fun (q, x) a -> Iterators.product (delta q a x) (Iterators.singleton ()))
		               state_iter alphabet_iter source source_iter
		               (Domain.make compare (fun _ -> "()")) (Iterators.singleton ()))
			(MetaTrapo.make alphabet states' initial' omega' omega_combine' (fun (q, x) a -> Iterators.product (delta' q a x) (Iterators.singleton ()))
		               state_iter' alphabet_iter source' source_iter'
		               (Domain.make compare (fun _ -> "()")) (Iterators.singleton ()))
		               
end;;


module RightMetaTaggedTrapo = struct

	type ('q, 'a, 'y, 'p, 'r) t = ('q, 'a, unit, 'y, 'p, unit, 'r) MetaTaggedTrapo.t

	let make alphabet states initial omega omega_combine delta state_iter alphabet_iter target target_iter
	         states' initial' omega' omega_combine' delta' state_iter' target' target_iter' =
		MetaTaggedTrapo.make
			(MetaTrapo.make alphabet states initial omega omega_combine (fun (q, _) a -> delta q a)
		               state_iter alphabet_iter (Domain.make compare (fun _ -> "()")) (Iterators.singleton ())
		               target target_iter)
			(MetaTrapo.make alphabet states' initial' omega' omega_combine' (fun (q, _) a -> delta' q a)
		               state_iter' alphabet_iter (Domain.make compare (fun _ -> "()")) (Iterators.singleton ())
		               target' target_iter')
		               
end;;


module SimpleTaggedTrapo = struct

	type ('q, 'a, 'p) t = ('q, 'a, unit, unit, 'p, unit, unit) TaggedTrapo.t
	
	let get_word = TaggedTrapo.get_word
	
	let decoration meta = let (p, _, i, p', _) = TaggedTrapo.decoration meta in (p, i, p')
	
	let is_idempotent = TaggedTrapo.is_idempotent
	
	let image meta (trapo, _) =
		SimpleTrapo.image (MetaTaggedTrapo.left meta) trapo
		
	let preimage meta (trapo, _) =
		SimpleTrapo.preimage (MetaTaggedTrapo.left meta) trapo

	let good_states meta (trapo, _) = SimpleTrapo.good_states (MetaTaggedTrapo.left meta) trapo
	
	let compare = TaggedTrapo.compare
	
	let compare_full = TaggedTrapo.compare_full
	
	let compare_transitions = TaggedTrapo.compare_transitions
	
	let compare_words = TaggedTrapo.compare_words
	
	let format = TaggedTrapo.format
	
	let format_word = TaggedTrapo.format_word
	
	let format_transitions = TaggedTrapo.format_transitions
	
	let single meta a p p' = TaggedTrapo.single meta a p () p' ()
	
	let compose meta = TaggedTrapo.compose meta meta (fun x -> x)
	
end	


module NpaMetaTaggedTrapo = struct

	type ('q, 'a, 'p) t = ('q, 'a, 'p) SimpleMetaTaggedTrapo.t

	let make npa1 state_iter1 npa2 state_iter2 alphabet_iter =
		SimpleMetaTaggedTrapo.make (NMA.alphabet npa1) (NMA.states npa1) (NMA.initial npa1) (NPA.omega npa1) Priorities.relevance_max (NMA.delta npa1) state_iter1 alphabet_iter (NMA.states npa2) (NMA.initial npa2) (NPA.omega npa2) Priorities.relevance_max (NMA.delta npa2) state_iter2

end;;



module FiniteNbaMetaTaggedTrapo = struct

	type ('q, 'a, 'p) t = ('q, 'a, 'p) SimpleMetaTaggedTrapo.t

	let make nba1 state_iter1 nba2 state_iter2 alphabet_iter =
		SimpleMetaTaggedTrapo.make (NMA.alphabet nba1) (NMA.states nba1) (NMA.initial nba1) (NPA.omega (NBA.asNPA nba1)) (fun left right -> right) (NMA.delta nba1) state_iter1 alphabet_iter (NMA.states nba2) (NMA.initial nba2) (NPA.omega (NBA.asNPA nba2)) (fun left right -> right) (NMA.delta nba2) state_iter2

end;;





module NpvpaMetaTaggedTrapo = struct

	type ('q, 'a, 'p) i = ('q, ('a NMVPA.nested), 'p) SimpleMetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) r = ('q, ('a NMVPA.nested), 's option, unit, 'p, 'r option, unit) MetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) c = ('q, ('a NMVPA.nested), unit, 's, 'p, unit, 'r) MetaTaggedTrapo.t
	
	let make_i npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter =
		SimpleMetaTaggedTrapo.make (NMVPA.alphabet npvpa1) (NMVPA.states npvpa1) (NMVPA.initial npvpa1) (NPVPA.omega npvpa1) Priorities.relevance_max (NMVPA.delta_internal npvpa1) state_iter1 (Iterators.filter (function NMVPA.Internal _ -> true | _ -> false) alphabet_iter) (NMVPA.states npvpa2) (NMVPA.initial npvpa2) (NPVPA.omega npvpa2) Priorities.relevance_max (NMVPA.delta_internal npvpa2) state_iter2
	
	let make_r npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2 =
		LeftMetaTaggedTrapo.make (NMVPA.alphabet npvpa1) (NMVPA.states npvpa1) (NMVPA.initial npvpa1) (NPVPA.omega npvpa1) Priorities.relevance_max (NMVPA.delta_pop npvpa1) state_iter1 (Iterators.filter (function NMVPA.Pop _ -> true | _ -> false) alphabet_iter) (Domain.option_domain (NMVPA.stack npvpa1)) (Iterators.option_iterator stack_iter1) (NMVPA.states npvpa2) (NMVPA.initial npvpa2) (NPVPA.omega npvpa2) Priorities.relevance_max (NMVPA.delta_pop npvpa2) state_iter2 (Domain.option_domain (NMVPA.stack npvpa2)) (Iterators.option_iterator stack_iter2)
	
	let make_c npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2 =
		RightMetaTaggedTrapo.make (NMVPA.alphabet npvpa1) (NMVPA.states npvpa1) (NMVPA.initial npvpa1) (NPVPA.omega npvpa1) Priorities.relevance_max (NMVPA.delta_push npvpa1) state_iter1 (Iterators.filter (function NMVPA.Push _ -> true | _ -> false) alphabet_iter) (NMVPA.stack npvpa1) stack_iter1 (NMVPA.states npvpa2) (NMVPA.initial npvpa2) (NPVPA.omega npvpa2) Priorities.relevance_max (NMVPA.delta_push npvpa2) state_iter2 (NMVPA.stack npvpa2) stack_iter2
		
	let make npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2 =
		(make_i npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter,
		 make_r npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2,
		 make_c npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2)

end


module FiniteNbvpaMetaTaggedTrapo = struct

	type ('q, 'a, 'p) i = ('q, ('a NMVPA.nested), 'p) SimpleMetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) r = ('q, ('a NMVPA.nested), 's option, unit, 'p, 'r option, unit) MetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) c = ('q, ('a NMVPA.nested), unit, 's, 'p, unit, 'r) MetaTaggedTrapo.t

	let make_i npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter =
		SimpleMetaTaggedTrapo.make (NMVPA.alphabet npvpa1) (NMVPA.states npvpa1) (NMVPA.initial npvpa1) (NPVPA.omega (NBVPA.asNPVPA npvpa1)) (fun left right -> right) (NMVPA.delta_internal npvpa1) state_iter1 (Iterators.filter (function NMVPA.Internal _ -> true | _ -> false) alphabet_iter) (NMVPA.states npvpa2) (NMVPA.initial npvpa2) (NPVPA.omega (NBVPA.asNPVPA npvpa2)) (fun left right -> right) (NMVPA.delta_internal npvpa2) state_iter2
	
	let make_r npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2 =
		LeftMetaTaggedTrapo.make (NMVPA.alphabet npvpa1) (NMVPA.states npvpa1) (NMVPA.initial npvpa1) (NPVPA.omega (NBVPA.asNPVPA npvpa1)) (fun left right -> right) (NMVPA.delta_pop npvpa1) state_iter1 (Iterators.filter (function NMVPA.Pop _ -> true | _ -> false) alphabet_iter) (Domain.option_domain (NMVPA.stack npvpa1)) (Iterators.option_iterator stack_iter1) (NMVPA.states npvpa2) (NMVPA.initial npvpa2) (NPVPA.omega (NBVPA.asNPVPA npvpa2)) (fun left right -> right) (NMVPA.delta_pop npvpa2) state_iter2 (Domain.option_domain (NMVPA.stack npvpa2)) (Iterators.option_iterator stack_iter2)
	
	let make_c npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2 =
		RightMetaTaggedTrapo.make (NMVPA.alphabet npvpa1) (NMVPA.states npvpa1) (NMVPA.initial npvpa1) (NPVPA.omega (NBVPA.asNPVPA npvpa1)) (fun left right -> right) (NMVPA.delta_push npvpa1) state_iter1 (Iterators.filter (function NMVPA.Push _ -> true | _ -> false) alphabet_iter) (NMVPA.stack npvpa1) stack_iter1 (NMVPA.states npvpa2) (NMVPA.initial npvpa2) (NPVPA.omega (NBVPA.asNPVPA npvpa2)) (fun left right -> right) (NMVPA.delta_push npvpa2) state_iter2 (NMVPA.stack npvpa2) stack_iter2
		
	let make npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2 =
		(make_i npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter,
		 make_r npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2,
		 make_c npvpa1 state_iter1 npvpa2 state_iter2 alphabet_iter stack_iter1 stack_iter2)

end

