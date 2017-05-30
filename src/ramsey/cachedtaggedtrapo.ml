open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsautohelper;;
open Tcsset;;
open Tcslist;;
open Tcstiming;;
open Tcscache;;
open Solverregistry;;
open Trapo;;
open Taggedtrapo;;
open Cachedtrapo;;


module CachedTaggedTrapo = struct

	type ('p, 'r, 's, 'a) s = (int * ('p * 'r * 'p * 's * int) * ('a Alphabet.word)) IntTypeCache.t
	
	let make meta s = 
		let right = MetaTaggedTrapo.right meta in
		let (p, r) = Iterators.first (MetaTrapo.state_source_iterator right) in
		IntTypeCache.make (Comparators.product3 compare (TaggedTrapo.compare_tagged_transitions meta) Comparators.nocompare) (0, (p, r, p, s, 0), [])
	
	let decode_right tagged_cache i =
		Triple.snd (IntTypeCache.decode tagged_cache i)
		
	let decode_left_index tagged_cache i =
		Triple.fst (IntTypeCache.decode tagged_cache i)
		
	let decode_left cache tagged_cache i =
		CachedTrapo.decode cache (decode_left_index tagged_cache i)
		
	let decode cache tagged_cache i =
		let (i, d, w) = IntTypeCache.decode tagged_cache i in
		TaggedTrapo.make (Trapo.set_word (CachedTrapo.decode cache i) w) d
	
	let decoration tagged_cache i =
		let (p, r, p', s, i) = decode_right tagged_cache i in
		(p,r,i,p',s)

	let single meta cache tagged_cache a p r p' s =
		let m = MetaTaggedTrapo.right meta in
		let t = CachedTrapo.single (MetaTaggedTrapo.left meta) cache a in
		IntTypeCache.encode tagged_cache
			(t,
			 (p, r, p', s, (MetaTrapo.omega_combine m (MetaTrapo.omega m p) (MetaTrapo.omega m p'))),
			 CachedTrapo.get_word cache t)

	let composable meta tagged_cache i j =
		let (_, _, q, _, _) = decode_right tagged_cache i in
		let (q', _, _, _, _) = decode_right tagged_cache j in
		(Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta))) q q' = 0

	let is_idempotent meta cache tagged_cache i =
		composable meta tagged_cache i i && CachedTrapo.is_idempotent (MetaTaggedTrapo.left meta) cache (decode_left_index tagged_cache i)

	let format meta cache tagged_cache i = TaggedTrapo.format meta (decode cache tagged_cache i)

	let get_word tagged_cache i = Triple.trd (IntTypeCache.decode tagged_cache i)
	
	let format_word meta cache tagged_cache i = TaggedTrapo.format_word meta (decode cache tagged_cache i)
	
	let image meta cache tagged_cache i = CachedTrapo.image (MetaTaggedTrapo.left meta) cache (decode_left_index tagged_cache i)	
	
	let preimage meta cache tagged_cache i = CachedTrapo.preimage (MetaTaggedTrapo.left meta) cache (decode_left_index tagged_cache i)

	let compose_by_composer meta_left tagged_cache_left tagged_cache_right tagged_cache composer i j =
			let (i', (q,r,_,_,p), w) = IntTypeCache.decode tagged_cache_left i in
			let (j', (_,_,q',s,p'), w') = IntTypeCache.decode tagged_cache_right j in
			let k' = composer i' j' in
			let x = (q,r,q',s, MetaTrapo.omega_combine (MetaTaggedTrapo.right meta_left) p p') in
			let k = IntTypeCache.encode tagged_cache (k', x, Alphabet.compose_words w w') in
			k

	let compose meta_left meta_right cache_left cache_right cache tagged_cache_left tagged_cache_right tagged_cache map_inner i j =
		let composer = CachedTrapo.compose (MetaTaggedTrapo.left meta_left) (MetaTaggedTrapo.left meta_right) cache_left cache_right cache map_inner in
		compose_by_composer meta_left tagged_cache_left tagged_cache_right tagged_cache composer i j
	
	let compose_right_by_composer meta_left tagged_cache_left tagged_cache_right composer i j =
			let (i', (q,r,_,s,p), w) = IntTypeCache.decode tagged_cache_left i in
			let (j', (_,_,q',_,p'), w') = IntTypeCache.decode tagged_cache_right j in
			let k' = composer i' j' in
			let x = (q,r,q',s, MetaTrapo.omega_combine (MetaTaggedTrapo.right meta_left) p p') in
			let k = IntTypeCache.encode tagged_cache_left (k', x, Alphabet.compose_words w w') in
			k

	let compose_right meta_left meta_right cache_left cache_right tagged_cache_left tagged_cache_right i j =
		let composer = CachedTrapo.compose_right (MetaTaggedTrapo.left meta_left) (MetaTaggedTrapo.left meta_right) cache_left cache_right in
		compose_right_by_composer meta_left tagged_cache_left tagged_cache_right composer i j


	let collapse_right meta meta_collapsed cache tagged_cache cache_collapsed tagged_cache_collapsed i = 
		let t = TaggedTrapo.collapse_right meta meta_collapsed (decode cache tagged_cache i) in
		IntTypeCache.encode tagged_cache_collapsed (
			CachedTrapo.encode cache_collapsed (TaggedTrapo.get_trapo t),
			(let (a,b,c,d,e) = TaggedTrapo.decoration t in (a,b,d,e,c)),
			get_word tagged_cache i
		)


	let project_left meta cache tagged_cache cache_projected tagged_cache_projected i symbol symbol'' = 
		match TaggedTrapo.project_left meta (decode cache tagged_cache i) symbol symbol'' with
		| None -> None
		| Some t -> Some (IntTypeCache.encode tagged_cache_projected (
			CachedTrapo.encode cache_projected (TaggedTrapo.get_trapo t),
			(let (a,b,c,d,e) = TaggedTrapo.decoration t in (a,b,d,e,c)),
			get_word tagged_cache i
		))

end	
			
	


module CachedTaggedTrapoSet = struct

	type ('p, 'r) t = ('p * 'r, int TreeSet.t) TreeMap.t
	
	let empty meta = TreeMap.empty (Domain.compare (MetaTrapo.state_source (MetaTaggedTrapo.right meta)))

	let is_empty = TreeMap.is_empty
	
	let cardinal m =
		let i = ref 0 in
		TreeMap.iter (fun _ inner ->
			i := !i + TreeSet.cardinal inner
		) m;
		!i
	
	let iter f = TreeMap.iter (fun _ -> TreeSet.iter f)
	
	let iter_sub f (q,r) tts =
		try
			TreeSet.iter f (TreeMap.find (q,r) tts)
		with
			Not_found -> ()
			
	let fold_sub f (q,r) tts acc =
		try
			TreeSet.fold f (TreeMap.find (q,r) tts) acc
		with
			Not_found -> acc

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
			
	let fold f = TreeMap.fold (fun _ -> TreeSet.fold f)

  let _add q r i tts =
		let set = 
			match TreeMap.find_opt (q,r) tts with
			| None -> TreeSet.empty_def
			| Some s -> s
		in
			TreeMap.add (q,r) (TreeSet.add i set) tts
			
	let add cache i set =
		let (q,r,_,_,_) = CachedTaggedTrapo.decoration cache i in
		_add q r i set  

	let singles meta cache tagged_cache =
		Iterators.fold (MetaTrapo.alphabet_iter (MetaTaggedTrapo.left meta)) (fun a ->
			Iterators.fold (MetaTrapo.state_source_iterator (MetaTaggedTrapo.right meta)) (fun (q,r) ->
				Iterators.fold (MetaTrapo.delta (MetaTaggedTrapo.right meta) (q,r) a) (fun (q',s) ->
					_add q r (CachedTaggedTrapo.single meta cache tagged_cache a q r q' s)
				)
			)
		) (empty meta)	
		

  let join composed_meta tagged_cache_left tagged_cache_right tagged_cache_composed inner_join composer left_set right_set =			
		fold (fun m acc ->
			let (_, _, _, p, s) = CachedTaggedTrapo.decoration tagged_cache_left m in
			fold (fun m' acc ->
				let (pp, ss, _, _, _) = CachedTaggedTrapo.decoration tagged_cache_right m' in
				if inner_join (p, s) (pp, ss)
				then let j = composer m m' in
				     let (q,r,_,_,_) = CachedTaggedTrapo.decoration tagged_cache_composed j in
				     _add q r j acc
				else acc
			) right_set acc
		) left_set (empty composed_meta)
	
end;;


module SimpleCachedTaggedTrapo = struct

	type ('p, 'a) s = ('p, unit, unit, 'a) CachedTaggedTrapo.s
	
	let make meta = 
		let right = MetaTaggedTrapo.right meta in
		let p = MetaTrapo.initial right in
		IntTypeCache.make (Comparators.product3 compare (TaggedTrapo.compare_tagged_transitions meta) Comparators.nocompare) (0, (p, (), p, (), 0), [])

	let format_word = CachedTaggedTrapo.format_word
	
	let format = CachedTaggedTrapo.format
	
	let get_word = CachedTaggedTrapo.get_word

	let decoration tagged_cache i = let (p, _, i, p', _) = CachedTaggedTrapo.decoration tagged_cache i in (p, i, p')
	
	let compose_by_composer meta tagged_cache = CachedTaggedTrapo.compose_by_composer meta tagged_cache tagged_cache tagged_cache

	let compose meta tagged_cache cache = CachedTaggedTrapo.compose meta meta cache cache cache tagged_cache tagged_cache tagged_cache (fun x -> x)
	
	let is_idempotent = CachedTaggedTrapo.is_idempotent
	
	let image meta tagged_cache cache i = SimpleCachedTrapo.image (MetaTaggedTrapo.left meta) cache (CachedTaggedTrapo.decode_left_index tagged_cache i)
	
	let preimage meta tagged_cache cache i = SimpleCachedTrapo.preimage (MetaTaggedTrapo.left meta) cache (CachedTaggedTrapo.decode_left_index tagged_cache i)
	
	let good_states meta tagged_cache cache i = SimpleCachedTrapo.good_states (MetaTaggedTrapo.left meta) cache (CachedTaggedTrapo.decode_left_index tagged_cache i)

end	


