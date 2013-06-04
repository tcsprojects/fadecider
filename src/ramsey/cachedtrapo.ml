open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsautohelper;;
open Tcsset;;
open Tcslist;;
open Tcstiming;;
open Tcscache;;
open Trapo;;


module CachedTrapo = struct

	type ('q, 'a, 'x, 'y) s = (('q, 'a, 'x, 'y) Trapo.t) IntTypeCache.t
	
	let make meta = IntTypeCache.make (Trapo.compare meta) (Trapo.single meta (Iterators.first (MetaTrapo.alphabet_iter meta)))
	
	let decode cache i = IntTypeCache.decode cache i
	
	let encode cache x = IntTypeCache.encode cache x
		
	let format_transitions meta cache i = Trapo.format_transitions meta (IntTypeCache.decode cache i)
	
	let format_word meta cache i = Trapo.format_word meta (IntTypeCache.decode cache i)
	
	let format meta cache i = Trapo.format meta (IntTypeCache.decode cache i)
		
	let get_word cache i = Trapo.get_word (IntTypeCache.decode cache i)
	
	let compare_words meta cache x y = Trapo.compare_words meta (IntTypeCache.decode cache x) (IntTypeCache.decode cache y)

	let compare_transitions meta cache x y = Trapo.compare_transitions meta (IntTypeCache.decode cache x) (IntTypeCache.decode cache y)

	let compare_full meta cache x y = Trapo.compare_full meta (IntTypeCache.decode cache x) (IntTypeCache.decode cache y)

	let compare meta cache x y = Trapo.compare meta (IntTypeCache.decode cache x) (IntTypeCache.decode cache y)

	let single meta cache a = IntTypeCache.encode cache (Trapo.single meta a)
	
	let is_idempotent meta cache i = Trapo.is_idempotent meta (IntTypeCache.decode cache i)
	
	let image meta cache i state_source_iterator = Trapo.image meta (IntTypeCache.decode cache i) state_source_iterator 

	let preimage meta cache i state_target_set = Trapo.preimage meta (IntTypeCache.decode cache i) state_target_set
	
	let compose meta_left meta_right cache_left cache_right cache map x y = IntTypeCache.encode cache (Trapo.compose meta_left meta_right map (IntTypeCache.decode cache_left x) (IntTypeCache.decode cache_right y))

	let compose_left meta_left meta_right cache_left cache_right x y = IntTypeCache.encode cache_right (Trapo.compose_left meta_left meta_right (IntTypeCache.decode cache_left x) (IntTypeCache.decode cache_right y))

	let compose_right meta_left meta_right cache_left cache_right x y = IntTypeCache.encode cache_left (Trapo.compose_right meta_left meta_right (IntTypeCache.decode cache_left x) (IntTypeCache.decode cache_right y))
	
	let collapse_right meta meta_collapsed cache cache_collapsed i =
		encode cache_collapsed (Trapo.collapse_right meta meta_collapsed (decode cache i))
		
  let project_left meta cache cache_projected i symbol =
		encode cache_projected (Trapo.project_left meta (decode cache i) symbol)		
		
end

	(*
module CachedTrapoComposer = struct
	
	type t = (int * int, int) TreeMap.t ref * (int ref * int ref * int ref)
	
	let empty _ = (ref TreeMap.empty_def, (ref 0, ref 0, ref 0))
	
	let compose (t,(a,l,m)) meta_left meta_right cache_left cache_right cache map x y =
		try
			incr a;
			let r = TreeMap.find (x,y) !t in
			incr l;
			r
		with Not_found ->
			incr m;
			let z = CachedTrapo.compose meta_left meta_right cache_left cache_right cache map x y in
			t := TreeMap.add (x,y) z !t;
			z
			
	let compose_left (t,(a,l,m)) meta_left meta_right cache_left cache_right x y =
		try
			incr a;
			let r = TreeMap.find (x,y) !t in
			incr l;
			r
		with Not_found ->
			incr m;
			let z = CachedTrapo.compose_left meta_left meta_right cache_left cache_right x y in
			t := TreeMap.add (x,y) z !t;
			z
	
	let compose_right (t,(a,l,m)) meta_left meta_right cache_left cache_right x y =
		try
			incr a;
			let r = TreeMap.find (x,y) !t in
			incr l;
			r
		with Not_found ->
			incr m;
			let z = CachedTrapo.compose_right meta_left meta_right cache_left cache_right x y in
			t := TreeMap.add (x,y) z !t;
			z

	let size (t,_) = TreeMap.cardinal !t
	let accesses (_,(a,_,_)) = !a
	let lookups (_, (_,l,_)) = !l
	let misses (_, (_,_,m)) = !m

end		

*)



module AssociativeHull = struct
	
  type t = (int, (int, int) TreeMap.t ref) TreeMap.t ref * (int, (int, int) TreeMap.t ref) TreeMap.t ref
	
	let empty _ = (ref TreeMap.empty_def, ref TreeMap.empty_def)
	
	let rec register (hull_x, hull_y) x y z =
		let registered = ref true in
		match TreeMap.find_opt x !hull_x with
		| Some y_map -> (
			match TreeMap.find_opt y !y_map with
			| Some z -> registered := false;
			| None -> y_map := TreeMap.add y z !y_map
		)
		| None -> hull_x := TreeMap.add x (ref (TreeMap.singleton_def y z)) !hull_x;
		if !registered then (
			match TreeMap.find_opt y !hull_y with
			| Some x_map -> x_map := TreeMap.add x z !x_map
			| None -> hull_y := TreeMap.add y (ref (TreeMap.singleton_def x z)) !hull_y
		);
		if !registered then propagate (hull_x, hull_y) x y z
		
	and propagate (hull_x, hull_y) x y z =
		let triples = ref [] in
		try
			let y_map = TreeMap.find y !hull_x in
			let z_map = TreeMap.find z !hull_x in
			TreeMap.iter (fun y' y'' ->
				match TreeMap.find_opt y' !z_map with
				| None -> ()
				| Some z'' -> triples := (x,y'',z'')::!triples 
			) !y_map;
    with Not_found -> ();
		try
			let x_map = TreeMap.find x !hull_y in
			let z_map = TreeMap.find z !hull_y in
			TreeMap.iter (fun x' x'' ->
				match TreeMap.find_opt x' !z_map with
				| None -> ()
				| Some z'' -> triples := (x'',y,z'')::!triples 
			) !x_map;
    with Not_found -> ();
		List.iter (fun (x',y',z') -> register (hull_x, hull_y) x' y' z') !triples
		
	let lookup (hull_x, hull_y) x y =
		TreeMap.find y (TreeMap.find x !hull_x)
		
	let lookup_revert (hull_x, hull_y) revert x y =
		match TreeMap.find_opt x !hull_x with
		| Some y_map -> (
			match TreeMap.find_opt y !y_map with
			| Some z -> z
			| None -> let z = revert x y in register (hull_x, hull_y) x y z; z
		)
		| None -> let z = revert x y in register (hull_x, hull_y) x y z; z
	
end

module CachedTrapoComposer = struct
	
	type t = AssociativeHull.t
	
	let empty = AssociativeHull.empty
	
	let compose t meta_left meta_right cache_left cache_right cache map =
		AssociativeHull.lookup_revert t (CachedTrapo.compose meta_left meta_right cache_left cache_right cache map)
			
	let compose_left t meta_left meta_right cache_left cache_right =
		AssociativeHull.lookup_revert t (CachedTrapo.compose_left meta_left meta_right cache_left cache_right)
	
	let compose_right t meta_left meta_right cache_left cache_right =
		AssociativeHull.lookup_revert t (CachedTrapo.compose_right meta_left meta_right cache_left cache_right)

end		
	
	
module CachedTrapoSet = struct

	type t = int TreeSet.t
	
	let to_trapo_set meta cache set = TreeSet.fold (fun i -> TrapoSet.add (IntTypeCache.decode cache i)) set (TrapoSet.empty meta)
	
	let from_trapo_set meta cache set = TrapoSet.fold (fun trapo -> TreeSet.add (IntTypeCache.encode cache trapo)) set TreeSet.empty_def

	let format meta cache set = TrapoSet.format meta (to_trapo_set meta cache set)
	
	let empty = TreeSet.empty_def
	
	let mem = TreeSet.mem
	
	let is_empty = TreeSet.is_empty
	
	let add = TreeSet.add

	let singles meta cache = from_trapo_set meta cache (TrapoSet.singles meta)
			
	let fold = TreeSet.fold
	
	let iter = TreeSet.iter

	let union = TreeSet.union
	
	let diff = TreeSet.diff
	
	let cardinal = TreeSet.cardinal
	
	let join composer left_set right_set =
		fold (fun m -> fold (fun m' -> add (composer m m')) right_set) left_set empty
	
	let conditional_join composer left_set right_set add_filter abort_condition =
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
		) left_set empty

end;;	
	
	


module SimpleCachedTrapo = struct

	type ('q, 'a) s = ('q, 'a, unit, unit) CachedTrapo.s
	
	let make = CachedTrapo.make
	
	let format_transitions = CachedTrapo.format_transitions
	
	let format_word = CachedTrapo.format_word
	
	let format = CachedTrapo.format
	
	let get_word = CachedTrapo.get_word

	let compare_words = CachedTrapo.compare_words
	
	let compare_transitions = CachedTrapo.compare_transitions
	
	let compare_full = CachedTrapo.compare_full

	let compare = CachedTrapo.compare

	let single = CachedTrapo.single
	
	let compose meta cache a b = CachedTrapo.compose meta meta cache cache cache (fun x -> x) a b

	let is_idempotent = CachedTrapo.is_idempotent

	let image meta cache x it = TreeSet.map2 (Domain.compare (MetaTrapo.states meta)) fst (CachedTrapo.image meta cache x (Iterators.product it (Iterators.singleton ())))

	let preimage meta cache x s = TreeSet.map2 (Domain.compare (MetaTrapo.states meta)) fst (CachedTrapo.preimage meta cache x (TreeSet.map2 (Domain.compare (MetaTrapo.state_source meta)) (fun x -> (x, ())) s))
	
	let epsilon meta cache = IntTypeCache.encode cache (SimpleTrapo.epsilon meta)
	
	let good_states meta cache i = SimpleTrapo.good_states meta (IntTypeCache.decode cache i)

end;;	
	