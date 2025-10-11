open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsautohelper;;
open Tcsset;;
open Tcslist;;
open Tcstiming;;
open Base;;
open Solverregistry;;
open Trapo;;
open Cachedtrapo;;

		



module TrapoComposers = struct
	
	let compose_cached_internal_internal meta_internal internal_cached_trapo =
			CachedTrapoComposer.compose
				(CachedTrapoComposer.empty ())
				meta_internal
				meta_internal
				internal_cached_trapo
				internal_cached_trapo
				internal_cached_trapo
				(fun x -> x)

	let compose_cached_call_return meta_call meta_ret call_cached_trapo return_cached_trapo internal_cached_trapo =
			CachedTrapoComposer.compose
			  (CachedTrapoComposer.empty ())
				meta_call
				meta_ret
				call_cached_trapo
				return_cached_trapo
				internal_cached_trapo
				(fun x -> Some x)
	
	let compose_cached_call_internal meta_call meta_internal call_cached_trapo internal_cached_trapo =
			CachedTrapoComposer.compose_right
			  (CachedTrapoComposer.empty ())
				meta_call
				meta_internal
				call_cached_trapo
				internal_cached_trapo
			
  let compose_internal_internal meta_internal =
		SimpleTrapo.compose meta_internal

  let compose_call_return meta_call meta_ret =
		Trapo.compose meta_call meta_ret (fun x -> Some x)
		 
	let compose_call_internal meta_call meta_internal =
		Trapo.compose_right meta_call meta_internal

end;;


module TrapoJoiners = struct
	
	let join_internal_internal_cached compose_internal_internal =
		CachedTrapoSet.join compose_internal_internal
	
	let join_call_return_cached compose_call_return = 
		CachedTrapoSet.join compose_call_return
			
	let join_call_internal_cached compose_call_internal =
	  CachedTrapoSet.join compose_call_internal

	let join_internal_internal meta_internal compose_internal_internal =
		TrapoSet.join meta_internal meta_internal meta_internal compose_internal_internal

	let join_call_return meta_call meta_ret meta_internal compose_call_return =
		TrapoSet.join meta_call meta_ret meta_internal compose_call_return
		
	let join_call_internal meta_call meta_internal compose_call_internal =
		TrapoSet.join meta_call meta_internal meta_call compose_call_internal

end;;


module NpaUniversality = struct

	type universality_info = {
			box_count: int;
		}
		
  let is_universal npa state_iterator alphabet_iterator =
		let meta_internal = NpaMetaTrapo.make npa state_iterator alphabet_iterator in


		let internal_cache = SimpleCachedTrapo.make meta_internal in
		let empty = CachedTrapoSet.empty in
		let is_empty = CachedTrapoSet.is_empty in
		let singles_internal = CachedTrapoSet.singles meta_internal internal_cache in
		let fold = CachedTrapoSet.fold in
		let is_idempotent = SimpleCachedTrapo.is_idempotent meta_internal internal_cache in
    let preimage = SimpleCachedTrapo.preimage meta_internal internal_cache in
		let good_states = SimpleCachedTrapo.good_states meta_internal internal_cache in
		let get_word = SimpleCachedTrapo.get_word internal_cache in
		let image = SimpleCachedTrapo.image meta_internal internal_cache in
		let union = CachedTrapoSet.union in
		let diff = CachedTrapoSet.diff in
		let cardinal = CachedTrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_cached_internal_internal meta_internal internal_cache in
		let join_internal_internal = TrapoJoiners.join_internal_internal_cached compose_internal_internal in
		(*
		let empty = TrapoSet.empty meta_internal in
		let is_empty = TrapoSet.is_empty in
		let singles_internal = TrapoSet.singles meta_internal in
		let fold = TrapoSet.fold in
		let is_idempotent = SimpleTrapo.is_idempotent meta_internal in
    let preimage =  SimpleTrapo.preimage meta_internal in
		let good_states = SimpleTrapo.good_states meta_internal in
		let get_word = SimpleTrapo.get_word in
		let image = SimpleTrapo.image meta_internal in
		let union = TrapoSet.union in
		let diff = TrapoSet.diff in
		let cardinal = TrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_internal_internal meta_internal in
		let join_internal_internal = TrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
		*)


		let new_internal = ref singles_internal in
		let boxes_internal = ref empty in
	  let bcount = ref (cardinal !new_internal) in
		let universal = ref None in
		let idempots = ref (StatesetSet.empty meta_internal) in
		let reachers = ref (StatesetSet.epsilon meta_internal) in
		while (!universal = None) && not (is_empty !new_internal) do
			let new_idempots = fold (fun m i ->
				if is_idempotent m then (
					let good = preimage m (good_states m) in
					if (StatesetSet.subsumed i good) || (StatesetSet.subsumed !idempots good) then i
					else StatesetSet.add i good (get_word m)
				)
				else i
			) !new_internal (StatesetSet.empty meta_internal) in
			(* Check new idempots with old reachers *)
			universal := StatesetSet.reacher_idempot_match meta_internal !reachers new_idempots;
			if !universal = None then (
				idempots := StatesetSet.union !idempots new_idempots;
				let new_reachers = fold (fun m r ->
					let image = image m (Iterators.singleton (MetaTrapo.initial meta_internal)) in
					if (StatesetSet.subsumed r image) || (StatesetSet.subsumed !reachers image) then r
					else StatesetSet.add r image (get_word m)
				) !new_internal (StatesetSet.empty meta_internal) in
				universal := StatesetSet.reacher_idempot_match meta_internal new_reachers !idempots;
				if !universal = None then (
					reachers := StatesetSet.union !reachers new_reachers;
					let boxes_internal' = union !boxes_internal !new_internal in
					let new_internal' = diff (
						union
							(join_internal_internal !new_internal boxes_internal')
							(join_internal_internal boxes_internal' !new_internal)
					) boxes_internal' in
					boxes_internal := boxes_internal';
					new_internal := new_internal';
					bcount := !bcount + cardinal !new_internal;
				)
			)
		done;
		({box_count = !bcount}, !universal)

end;;





module FiniteNbaUniversality = struct

	type universality_info = {
			box_count: int;
		}
		
  let is_universal nba state_iterator alphabet_iterator =
		let meta_internal = FiniteNbaMetaTrapo.make nba state_iterator alphabet_iterator in


		let internal_cache = SimpleCachedTrapo.make meta_internal in
		let empty = CachedTrapoSet.empty in
		let is_empty = CachedTrapoSet.is_empty in
		let singles_internal = CachedTrapoSet.singles meta_internal internal_cache in
		let iter = CachedTrapoSet.iter in
		let format_word = SimpleCachedTrapo.format_word meta_internal internal_cache in
		let image = SimpleCachedTrapo.image meta_internal internal_cache in
		let union = CachedTrapoSet.union in
		let diff = CachedTrapoSet.diff in
		let cardinal = CachedTrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_cached_internal_internal meta_internal internal_cache in
		let join_internal_internal = TrapoJoiners.join_internal_internal_cached compose_internal_internal in
		(*
		let empty = TrapoSet.empty meta_internal in
		let is_empty = TrapoSet.is_empty in
		let singles_internal = TrapoSet.singles meta_internal in
		let iter = TrapoSet.iter in
		let format_word = SimpleTrapo.format_word meta_internal in
		let image = SimpleTrapo.image meta_internal in
		let union = TrapoSet.union in
		let diff = TrapoSet.diff in
		let cardinal = TrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_internal_internal meta_internal in
		let join_internal_internal = TrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
		*)


		let new_internal = ref singles_internal in
		let boxes_internal = ref empty in
	  let bcount = ref (cardinal !new_internal) in
		let universal = ref None in
		while (!universal = None) && not (is_empty !new_internal) do
			iter (fun box ->
				if !universal = None then (
					let img = image box (Iterators.singleton (MetaTrapo.initial meta_internal)) in
					if not (TreeSet.exists (NBA.accept nba) img)
					then universal := Some (format_word box)
				)
			) !new_internal;
			if !universal = None then (
				let boxes_internal' = union !boxes_internal !new_internal in
				let new_internal' = diff (
					union
						(join_internal_internal !new_internal boxes_internal')
						(join_internal_internal boxes_internal' !new_internal)
				) boxes_internal' in
				boxes_internal := boxes_internal';
				new_internal := new_internal';
				bcount := !bcount + cardinal !new_internal;
			)
		done;
		({box_count = !bcount}, !universal)

end;;






module NpvpaUniversality = struct

	type universality_info = {
			box_count: int;
		}

  let is_universal npvpa state_iterator alphabet_iterator stack_iterator =
	
		let (meta_internal, meta_ret, meta_call) = NpvpaMetaTrapo.make npvpa state_iterator alphabet_iterator stack_iterator in
	
		let internal_cache = SimpleCachedTrapo.make meta_internal in
		let return_cache = CachedTrapo.make meta_ret in
		let call_cache = CachedTrapo.make meta_call in
		let empty = CachedTrapoSet.empty in
		let is_empty = CachedTrapoSet.is_empty in
		let singles_internal = CachedTrapoSet.singles meta_internal internal_cache in
		let singles_call = CachedTrapoSet.singles meta_call call_cache in
		let singles_ret = CachedTrapoSet.singles meta_ret return_cache in
		let fold = CachedTrapoSet.fold in
		let is_idempotent = SimpleCachedTrapo.is_idempotent meta_internal internal_cache in
    let preimage = SimpleCachedTrapo.preimage meta_internal internal_cache in
		let good_states = SimpleCachedTrapo.good_states meta_internal internal_cache in
		let get_word = SimpleCachedTrapo.get_word internal_cache in
		let add = CachedTrapoSet.add in
		let image = SimpleCachedTrapo.image meta_internal internal_cache in
		let union = CachedTrapoSet.union in
		let diff = CachedTrapoSet.diff in
		let cardinal = CachedTrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_cached_internal_internal meta_internal internal_cache in
		let compose_call_return = TrapoComposers.compose_cached_call_return meta_call meta_ret call_cache return_cache internal_cache in
		let compose_call_internal = TrapoComposers.compose_cached_call_internal meta_call meta_internal call_cache internal_cache in
		let join_internal_internal = TrapoJoiners.join_internal_internal_cached compose_internal_internal in
		let join_call_return = TrapoJoiners.join_call_return_cached compose_call_return in
		let join_call_internal = TrapoJoiners.join_call_internal_cached compose_call_internal in
		let collapse_right = CachedTrapo.collapse_right meta_call meta_internal call_cache internal_cache in
		let project_left = CachedTrapo.project_left meta_ret return_cache internal_cache in
	  (*
		let empty = TrapoSet.empty meta_internal in
		let is_empty = TrapoSet.is_empty in
		let singles_internal = TrapoSet.singles meta_internal in
		let singles_call = TrapoSet.singles meta_call in
		let singles_ret = TrapoSet.singles meta_ret in
		let fold = TrapoSet.fold in
		let is_idempotent = SimpleTrapo.is_idempotent meta_internal in
    let preimage =  SimpleTrapo.preimage meta_internal in
		let good_states = SimpleTrapo.good_states meta_internal in
		let get_word = SimpleTrapo.get_word in
		let image = SimpleTrapo.image meta_internal in
		let add = TrapoSet.add in
		let union = TrapoSet.union in
		let diff = TrapoSet.diff in
		let cardinal = TrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_internal_internal meta_internal in
		let compose_call_return = TrapoComposers.compose_call_return meta_call meta_ret in
		let compose_call_internal = TrapoComposers.compose_call_internal meta_call meta_internal in
		let join_internal_internal = TrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
		let join_call_return = TrapoJoiners.join_call_return meta_call meta_ret meta_internal compose_call_return	in
		let join_call_internal = TrapoJoiners.join_call_internal meta_call meta_internal compose_call_internal in		
		let collapse_right = Trapo.collapse_right meta_call meta_internal in
		let project_left = Trapo.project_left meta_ret in
		*)
		
					
		let universal = ref None in
		let singles_internal = union (join_call_return singles_call singles_ret) singles_internal in
		let new_internal = ref singles_internal in
		let boxes_internal = ref empty in
	  let bcount = ref (cardinal !new_internal) in
    let idempots = ref (StatesetSet.empty meta_internal) in
		let reachers = ref (StatesetSet.epsilon meta_internal) in

		
		let update_and_check_universal set =
			let new_idempots = fold (fun m i ->
				if is_idempotent m then (
					let good = preimage m (good_states m) in
					if (StatesetSet.subsumed i good) || (StatesetSet.subsumed !idempots good) then i
					else StatesetSet.add i good (get_word m)
				)
				else i
			) set (StatesetSet.empty meta_internal) in
			universal := StatesetSet.reacher_idempot_match meta_internal !reachers new_idempots;
			if !universal = None then (
				idempots := StatesetSet.union !idempots new_idempots;
				let new_reachers = fold (fun m r ->
					let image = image m (Iterators.singleton (MetaTrapo.initial meta_internal)) in
					if (StatesetSet.subsumed r image) || (StatesetSet.subsumed !reachers image) then r
					else StatesetSet.add r image (get_word m)
				) set (StatesetSet.empty meta_internal) in
				universal := StatesetSet.reacher_idempot_match meta_internal new_reachers !idempots;		
				if !universal = None then reachers := StatesetSet.union !reachers new_reachers
			)
		in		
						
		while (!universal = None) && not (is_empty !new_internal) do
			update_and_check_universal !new_internal;
			if !universal = None then (
				let boxes_internal' = union !boxes_internal !new_internal in
				let new_internal' = diff (
					union
						(union 
							(join_internal_internal !new_internal boxes_internal')
							(join_internal_internal boxes_internal' !new_internal)
						)
						(join_call_return (join_call_internal singles_call !new_internal) singles_ret)
				) boxes_internal' in
				boxes_internal := boxes_internal';
				new_internal := new_internal';
				bcount := !bcount + cardinal !new_internal;
			)
		done;
		if (!universal = None) && !check_call_collapsed then (
			let singles_call_collapsed = fold (fun trapo -> add (collapse_right trapo)) singles_call empty in
			let new_call_collapsed = ref singles_call_collapsed in
			let call_collapsed = ref !boxes_internal in
			while (!universal = None) && not (is_empty !new_call_collapsed) do
			  update_and_check_universal !new_call_collapsed;
				if !universal = None then (
					let call_collapsed' = union !call_collapsed !new_call_collapsed in
					let new_call_collapsed' = diff (
						union
							(join_internal_internal !new_call_collapsed call_collapsed')
							(join_internal_internal call_collapsed' !new_call_collapsed)
					) call_collapsed' in
					call_collapsed := call_collapsed';
					new_call_collapsed := new_call_collapsed';
					bcount := !bcount + cardinal !new_call_collapsed;
				)
			done;
		);
		if (!universal = None) && !check_ret_projected then (
  		let singles_ret_projected = fold (fun trapo -> add (project_left trapo None)) singles_ret empty in
			let new_ret_projected = ref singles_ret_projected in
			let ret_projected = ref !boxes_internal in
			while (!universal = None) && not (is_empty !new_ret_projected) do
			  update_and_check_universal !new_ret_projected;
				if !universal = None then (
					let ret_projected' = union !ret_projected !new_ret_projected in
					let new_ret_projected' = diff (
						union
							(join_internal_internal !new_ret_projected ret_projected')
							(join_internal_internal ret_projected' !new_ret_projected)
					) ret_projected' in
					ret_projected := ret_projected';
					new_ret_projected := new_ret_projected';
					bcount := !bcount + cardinal !new_ret_projected;
				)
			done;
		);
		({box_count = !bcount}, !universal)

end;;








module FiniteNbvpaUniversality = struct

	type universality_info = {
			box_count: int;
		}

  let is_universal nbvpa state_iterator alphabet_iterator stack_iterator =
		(* Special case: epsilon *)
		if not (NBVPA.accept nbvpa (NMVPA.initial nbvpa))
		then ({box_count = 0}, Some "")
		else
			let (meta_internal, meta_ret, meta_call) = FiniteNbvpaMetaTrapo.make nbvpa state_iterator alphabet_iterator stack_iterator in
			
		
		let internal_cache = SimpleCachedTrapo.make meta_internal in
		let return_cache = CachedTrapo.make meta_ret in
		let call_cache = CachedTrapo.make meta_call in
		let empty = CachedTrapoSet.empty in
		let is_empty = CachedTrapoSet.is_empty in
		let singles_internal = CachedTrapoSet.singles meta_internal internal_cache in
		let singles_call = CachedTrapoSet.singles meta_call call_cache in
		let singles_ret = CachedTrapoSet.singles meta_ret return_cache in
		let iter = CachedTrapoSet.iter in
		let fold = CachedTrapoSet.fold in
		let add = CachedTrapoSet.add in
		let format_word = SimpleCachedTrapo.format_word meta_internal internal_cache in
		let image = SimpleCachedTrapo.image meta_internal internal_cache in
		let union = CachedTrapoSet.union in
		let diff = CachedTrapoSet.diff in
		let cardinal = CachedTrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_cached_internal_internal meta_internal internal_cache in
		let compose_call_return = TrapoComposers.compose_cached_call_return meta_call meta_ret call_cache return_cache internal_cache in
		let compose_call_internal = TrapoComposers.compose_cached_call_internal meta_call meta_internal call_cache internal_cache in
		let join_internal_internal = TrapoJoiners.join_internal_internal_cached compose_internal_internal in
		let join_call_return = TrapoJoiners.join_call_return_cached compose_call_return in
		let join_call_internal = TrapoJoiners.join_call_internal_cached compose_call_internal in
		let collapse_right = CachedTrapo.collapse_right meta_call meta_internal call_cache internal_cache in
		let project_left = CachedTrapo.project_left meta_ret return_cache internal_cache in
	  (*
		let empty = TrapoSet.empty meta_internal in
		let is_empty = TrapoSet.is_empty in
		let singles_internal = TrapoSet.singles meta_internal in
		let singles_call = TrapoSet.singles meta_call in
		let singles_ret = TrapoSet.singles meta_ret in
		let iter = TrapoSet.iter in
		let fold = TrapoSet.fold in
		let add = TrapoSet.add in
		let format_word = SimpleTrapo.format_word meta_internal in
		let image = SimpleTrapo.image meta_internal in
		let union = TrapoSet.union in
		let diff = TrapoSet.diff in
		let cardinal = TrapoSet.cardinal in
		let compose_internal_internal = TrapoComposers.compose_internal_internal meta_internal in
		let compose_call_return = TrapoComposers.compose_call_return meta_call meta_ret in
		let compose_call_internal = TrapoComposers.compose_call_internal meta_call meta_internal in
		let join_internal_internal = TrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
		let join_call_return = TrapoJoiners.join_call_return meta_call meta_ret meta_internal compose_call_return	in
		let join_call_internal = TrapoJoiners.join_call_internal meta_call meta_internal compose_call_internal in		
		let collapse_right = Trapo.collapse_right meta_call meta_internal in
		let project_left = Trapo.project_left meta_ret in
		*)
		
		
		let test_universal set =
			let universal = ref None in
			iter (fun box ->
				if !universal = None then (
					let img = image box (Iterators.singleton (MetaTrapo.initial meta_internal)) in
					if not (TreeSet.exists (NBVPA.accept nbvpa) img)
					then universal := Some (format_word box)
				)
			) set;
			!universal
		in
		
					
		let universal = ref None in
		let singles_internal = union (join_call_return singles_call singles_ret) singles_internal in
		let new_internal = ref singles_internal in
		let boxes_internal = ref empty in
	  let bcount = ref (cardinal !new_internal) in
		
		while (!universal = None) && not (is_empty !new_internal) do
				universal := test_universal !new_internal;
				if !universal = None then (
					let boxes_internal' = union !boxes_internal !new_internal in
					let new_internal' = diff (
						union
							(union 
								(join_internal_internal !new_internal boxes_internal')
								(join_internal_internal boxes_internal' !new_internal)
							)
							(join_call_return (join_call_internal singles_call !new_internal) singles_ret)
					) boxes_internal' in
					boxes_internal := boxes_internal';
					new_internal := new_internal';
					bcount := !bcount + cardinal !new_internal;
				);
		done;
		if (!universal = None) && !check_call_collapsed then (
			let singles_call_collapsed = fold (fun trapo -> add (collapse_right trapo)) singles_call empty in
			let new_call_collapsed = ref singles_call_collapsed in
			let call_collapsed = ref !boxes_internal in
			while (!universal = None) && not (is_empty !new_call_collapsed) do
			  universal := test_universal !new_call_collapsed;
				if !universal = None then (
					let call_collapsed' = union !call_collapsed !new_call_collapsed in
					let new_call_collapsed' = diff (
						union
							(join_internal_internal !new_call_collapsed call_collapsed')
							(join_internal_internal call_collapsed' !new_call_collapsed)
					) call_collapsed' in
					call_collapsed := call_collapsed';
					new_call_collapsed := new_call_collapsed';
					bcount := !bcount + cardinal !new_call_collapsed;
				)
			done;
		);
		if (!universal = None) && !check_ret_projected then (
  		let singles_ret_projected = fold (fun trapo -> add (project_left trapo None)) singles_ret empty in
			let new_ret_projected = ref singles_ret_projected in
			let ret_projected = ref !boxes_internal in
			while (!universal = None) && not (is_empty !new_ret_projected) do
			  universal := test_universal !new_ret_projected;
				if !universal = None then (
					let ret_projected' = union !ret_projected !new_ret_projected in
					let new_ret_projected' = diff (
						union
							(join_internal_internal !new_ret_projected ret_projected')
							(join_internal_internal ret_projected' !new_ret_projected)
					) ret_projected' in
					ret_projected := ret_projected';
					new_ret_projected := new_ret_projected';
					bcount := !bcount + cardinal !new_ret_projected;
				)
			done;
		);
		({box_count = !bcount}, !universal)

end;;



let register _ =
UniversalitySolvers.register {
	UniversalitySolvers.ident = "npa";
	UniversalitySolvers.description = "npa universality test";
	UniversalitySolvers.automata_class = NpaClass;
	UniversalitySolvers.solve = (function (NpaType (npa, states, alpha)) -> (
					let f info _ = "Info: " ^ string_of_int (info.NpaUniversality.box_count) ^ " boxes\n" in
					match NpaUniversality.is_universal npa states alpha with
						(info, None) -> (UniversalitySolvers.Universal, f info)
					|	(info, Some w) -> (UniversalitySolvers.NotAccepting w, f info)
				)
	          | _ -> failwith "wrong automaton class");
};
UniversalitySolvers.register {
	UniversalitySolvers.ident = "finite_nba";
	UniversalitySolvers.description = "finite nba universality test";
	UniversalitySolvers.automata_class = NbaClass;
	UniversalitySolvers.solve = (function (NbaType (nba, states, alpha)) -> (
					let f info _ = "Info: " ^ string_of_int (info.FiniteNbaUniversality.box_count) ^ " boxes\n" in
					match FiniteNbaUniversality.is_universal nba states alpha with
						(info, None) -> (UniversalitySolvers.Universal, f info)
					|	(info, Some w) -> (UniversalitySolvers.NotAccepting w, f info)
				)
	          | _ -> failwith "wrong automaton class");
};
UniversalitySolvers.register {
	UniversalitySolvers.ident = "npvpa";
	UniversalitySolvers.description = "npvpa universality test";
	UniversalitySolvers.automata_class = NpvpaClass;
	UniversalitySolvers.solve = (function (NpvpaType (npvpa, states, alpha, stack)) -> (
					let f info _ = "Info: " ^ string_of_int (info.NpvpaUniversality.box_count) ^ " boxes\n" in
					match NpvpaUniversality.is_universal npvpa states alpha stack with
						(info, None) -> (UniversalitySolvers.Universal, f info)
					|	(info, Some w) -> (UniversalitySolvers.NotAccepting w, f info)
				)
	          | _ -> failwith "wrong automaton class");
};
UniversalitySolvers.register {
	UniversalitySolvers.ident = "finite_nbvpa";
	UniversalitySolvers.description = "finite nbvpa universality test";
	UniversalitySolvers.automata_class = NbvpaClass;
	UniversalitySolvers.solve = (function (NbvpaType (nbvpa, states, alpha, stack)) -> (
					let f info _ = "Info: " ^ string_of_int (info.FiniteNbvpaUniversality.box_count) ^ " boxes\n" in
					match FiniteNbvpaUniversality.is_universal nbvpa states alpha stack with
						(info, None) -> (UniversalitySolvers.Universal, f info)
					|	(info, Some w) -> (UniversalitySolvers.NotAccepting w, f info)
				)
	          | _ -> failwith "wrong automaton class");
};;

