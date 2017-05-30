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
open Taggedtrapo;;
open Cachedtrapo;;
open Cachedtaggedtrapo;;



module TaggedTrapoCaches = struct
	
	let internal_caches meta = (
		SimpleCachedTrapo.make (MetaTaggedTrapo.left meta), 
		SimpleCachedTaggedTrapo.make meta
	)
		
	let return_caches meta = (
		CachedTrapo.make (MetaTaggedTrapo.left meta),
		CachedTaggedTrapo.make meta ()
	)
	
	let call_caches meta = (
		CachedTrapo.make (MetaTaggedTrapo.left meta),
		CachedTaggedTrapo.make meta 0
	)
	
end;;

module TaggedTrapoComposers = struct
	
	let compose_cached_internal_internal_cached meta_internal (internal_cached_trapo, internal_cached_tagged_trapo) =
			let internal_internal_composer =
				CachedTrapoComposer.compose
					(CachedTrapoComposer.empty ())
					(MetaTaggedTrapo.left meta_internal)
					(MetaTaggedTrapo.left meta_internal)
					internal_cached_trapo
					internal_cached_trapo
					internal_cached_trapo
					(fun x -> x)
			in
		  SimpleCachedTaggedTrapo.compose_by_composer meta_internal	internal_cached_tagged_trapo	internal_internal_composer
		
	let compose_cached_internal_internal meta_internal (internal_cached_trapo, internal_cached_tagged_trapo) =
		  SimpleCachedTaggedTrapo.compose meta_internal internal_cached_tagged_trapo internal_cached_trapo		
		
	let compose_cached_call_return_cached meta_call meta_ret (call_cached_trapo, call_cached_tagged_trapo) (return_cached_trapo, return_cached_tagged_trapo) (internal_cached_trapo, internal_cached_tagged_trapo) =
			let call_return_composer =
				CachedTrapoComposer.compose
				  (CachedTrapoComposer.empty ())
					(MetaTaggedTrapo.left meta_call)
					(MetaTaggedTrapo.left meta_ret)
					call_cached_trapo
					return_cached_trapo
					internal_cached_trapo
					(fun x -> Some x)
			in
		  CachedTaggedTrapo.compose_by_composer meta_call call_cached_tagged_trapo return_cached_tagged_trapo internal_cached_tagged_trapo call_return_composer

	let compose_cached_call_return meta_call meta_ret (call_cached_trapo, call_cached_tagged_trapo) (return_cached_trapo, return_cached_tagged_trapo) (internal_cached_trapo, internal_cached_tagged_trapo) = 
		  CachedTaggedTrapo.compose meta_call meta_ret call_cached_trapo return_cached_trapo internal_cached_trapo call_cached_tagged_trapo return_cached_tagged_trapo internal_cached_tagged_trapo (fun x -> Some x)
						
	let compose_cached_call_internal_cached meta_call meta_internal (call_cached_trapo, call_cached_tagged_trapo) (internal_cached_trapo, internal_cached_tagged_trapo) =
			let call_internal_composer =
				CachedTrapoComposer.compose_right
				  (CachedTrapoComposer.empty ())
					(MetaTaggedTrapo.left meta_call)
					(MetaTaggedTrapo.left meta_internal)
					call_cached_trapo
					internal_cached_trapo
			in
		  CachedTaggedTrapo.compose_right_by_composer meta_call call_cached_tagged_trapo internal_cached_tagged_trapo call_internal_composer
			

	let compose_cached_call_internal meta_call meta_internal (call_cached_trapo, call_cached_tagged_trapo) (internal_cached_trapo, internal_cached_tagged_trapo) =
		  CachedTaggedTrapo.compose_right meta_call meta_internal	call_cached_trapo internal_cached_trapo	call_cached_tagged_trapo internal_cached_tagged_trapo

  let compose_internal_internal meta_internal =
		SimpleTaggedTrapo.compose meta_internal
		
  let compose_call_return meta_call meta_ret =
		TaggedTrapo.compose meta_call meta_ret (fun x -> Some x)
		 
	let compose_call_internal meta_call meta_internal =
		TaggedTrapo.compose_right meta_call meta_internal
		
end;;


module TaggedTrapoJoiners = struct
	
	let join_internal_internal_cached meta_internal (internal_cached_trapo, internal_cached_tagged_trapo) compose_internal_internal =
		let cmp = Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta_internal)) in
		CachedTaggedTrapoSet.join meta_internal internal_cached_tagged_trapo internal_cached_tagged_trapo internal_cached_tagged_trapo (fun (x,_) (y,_) -> cmp x y = 0) compose_internal_internal
	
	let join_call_return_cached meta_internal meta_ret (call_cached_trapo, call_cached_tagged_trapo) (return_cached_trapo, return_cached_tagged_trapo) (internal_cached_trapo, internal_cached_tagged_trapo) compose_call_return = 
		let cmp = Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta_internal)) in
    let cmp' = Domain.compare (MetaTrapo.source (MetaTaggedTrapo.right meta_ret)) in
		CachedTaggedTrapoSet.join meta_internal call_cached_tagged_trapo return_cached_tagged_trapo internal_cached_tagged_trapo (fun (x,y) (x',y') -> (cmp x x' = 0) && (cmp' (Some y) y' = 0)) compose_call_return
			
	let join_call_internal_cached meta_call meta_internal (call_cached_trapo, call_cached_tagged_trapo) (internal_cached_trapo, internal_cached_tagged_trapo) compose_call_internal =
		let cmp = Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta_internal)) in
	  CachedTaggedTrapoSet.join meta_call call_cached_tagged_trapo internal_cached_tagged_trapo call_cached_tagged_trapo (fun (x,_) (y,_) -> cmp x y = 0) compose_call_internal

	let join_internal_internal meta_internal compose_internal_internal =
		let cmp = Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta_internal)) in
		TaggedTrapoSet.join meta_internal (fun (x,_) (y,_) -> cmp x y = 0) compose_internal_internal
		
	let join_call_return meta_internal meta_ret compose_call_return =
		let cmp = Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta_internal)) in
    let cmp' = Domain.compare (MetaTrapo.source (MetaTaggedTrapo.right meta_ret)) in
		TaggedTrapoSet.join meta_internal (fun (x,y) (x',y') -> (cmp x x' = 0) && (cmp' (Some y) y' = 0)) compose_call_return
		
	let join_call_internal meta_call meta_internal compose_call_internal=
		let cmp = Domain.compare (MetaTrapo.states (MetaTaggedTrapo.right meta_internal)) in
		TaggedTrapoSet.join meta_call (fun (x,_) (y,_) -> cmp x y = 0) compose_call_internal
		
end;;



module NpaSubsumption = struct

	type subsumption_info = {
			box_count: int;
		}

  let is_subsumed npa2 npa1 iter2 iter1 alphaiter =
		let meta_internal = NpaMetaTaggedTrapo.make npa1 iter1 npa2 iter2 alphaiter in

		let ((internal_cached_trapo, internal_cached_tagged_trapo) as internal_cache) = TaggedTrapoCaches.internal_caches meta_internal in
		let compose_internal_internal = TaggedTrapoComposers.compose_cached_internal_internal_cached meta_internal internal_cache in
		let join_internal_internal = TaggedTrapoJoiners.join_internal_internal_cached meta_internal internal_cache compose_internal_internal	in
		let union = CachedTaggedTrapoSet.union in
		let diff = CachedTaggedTrapoSet.diff in
		let cardinal = CachedTaggedTrapoSet.cardinal in
		let fold = CachedTaggedTrapoSet.fold in
		let image = SimpleCachedTaggedTrapo.image meta_internal internal_cached_tagged_trapo internal_cached_trapo in
    let preimage = SimpleCachedTaggedTrapo.preimage meta_internal internal_cached_tagged_trapo internal_cached_trapo in
		let decoration = SimpleCachedTaggedTrapo.decoration internal_cached_tagged_trapo in
		let get_word = SimpleCachedTaggedTrapo.get_word internal_cached_tagged_trapo in
		let fold_sub = CachedTaggedTrapoSet.fold_sub in
		let is_idempotent = SimpleCachedTaggedTrapo.is_idempotent meta_internal internal_cached_trapo internal_cached_tagged_trapo in
		let empty = CachedTaggedTrapoSet.empty meta_internal in
		let good_states = SimpleCachedTaggedTrapo.good_states meta_internal internal_cached_tagged_trapo internal_cached_trapo in
		let is_empty = CachedTaggedTrapoSet.is_empty in
		let singles_internal = CachedTaggedTrapoSet.singles meta_internal internal_cached_trapo internal_cached_tagged_trapo in
		
		(*
		let compose_internal_internal = TaggedTrapoComposers.compose_internal_internal meta_internal in
		let join_internal_internal = TaggedTrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
		let union = TaggedTrapoSet.union in
		let diff = TaggedTrapoSet.diff in
		let cardinal = TaggedTrapoSet.cardinal in
		let image = SimpleTaggedTrapo.image meta_internal in
    let preimage = SimpleTaggedTrapo.preimage meta_internal in
		let decoration = SimpleTaggedTrapo.decoration in
		let get_word = SimpleTaggedTrapo.get_word in
		let fold_sub = TaggedTrapoSet.fold_sub in
		let fold = TaggedTrapoSet.fold in
		let is_idempotent = SimpleTaggedTrapo.is_idempotent meta_internal in
		let empty = TaggedTrapoSet.empty meta_internal in
		let good_states = SimpleTaggedTrapo.good_states meta_internal in
		let is_empty = TaggedTrapoSet.is_empty in
		let singles_internal = TaggedTrapoSet.singles meta_internal in
		*)

		let new_internal = ref singles_internal in
		let boxes_internal = ref empty in
	  let bcount = ref (cardinal !new_internal) in

		let subsumed = ref None in

		let idempots = ref (TaggedStatesetSet.empty meta_internal) in
		let reachers = ref (TaggedStatesetSet.epsilon meta_internal) in

    while (!subsumed = None) && not (is_empty !new_internal) do
			let new_idempots = fold (fun m i ->
				let (r, p, _) = decoration m in
				if (p mod 2 = 0) && (is_idempotent m) then (
					let good = preimage m (good_states m) in
					if (TaggedStatesetSet.subsumed i r good) || (TaggedStatesetSet.subsumed !idempots r good) then i
					else TaggedStatesetSet.add meta_internal i r good (get_word m)
				)
				else i
			) !new_internal (TaggedStatesetSet.empty meta_internal) in
			subsumed := TaggedStatesetSet.reacher_idempot_match meta_internal !reachers new_idempots;
			if !subsumed = None then (
				idempots := TaggedStatesetSet.union !idempots new_idempots;
				let new_reachers = fold_sub (fun m r ->
					let (_,_,q) = decoration m in
					let image = image m (Iterators.singleton (MetaTrapo.initial (MetaTaggedTrapo.left meta_internal))) in
					if (TaggedStatesetSet.subsumed r q image) ||
					   (TaggedStatesetSet.subsumed !reachers q image) then r
					else TaggedStatesetSet.add meta_internal r q image (get_word m)
				) ((MetaTrapo.initial (MetaTaggedTrapo.right meta_internal)), ()) !new_internal (TaggedStatesetSet.empty meta_internal) in
				subsumed := TaggedStatesetSet.reacher_idempot_match meta_internal new_reachers !idempots;
				if !subsumed = None then (
					reachers := TaggedStatesetSet.union !reachers new_reachers;
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
		({ box_count = !bcount}, !subsumed)

end;;







module FiniteNbaSubsumption = struct

	type subsumption_info = {
			box_count: int;
		}
		
    let is_subsumed nba2 nba1 iter2 iter1 alphaiter =
		(* Special case: epsilon *)
		if (NBA.accept nba2 (NMA.initial nba2)) && not (NBA.accept nba1 (NMA.initial nba1))
		then ({box_count = 0}, Some "")
		else
			let meta_internal = FiniteNbaMetaTaggedTrapo.make nba1 iter1 nba2 iter2 alphaiter in
			
			let ((internal_cached_trapo, internal_cached_tagged_trapo) as internal_cache) = TaggedTrapoCaches.internal_caches meta_internal in
			let compose_internal_internal = TaggedTrapoComposers.compose_cached_internal_internal_cached meta_internal internal_cache in
			let join_internal_internal = TaggedTrapoJoiners.join_internal_internal_cached meta_internal internal_cache compose_internal_internal	in
			let union = CachedTaggedTrapoSet.union in
			let diff = CachedTaggedTrapoSet.diff in
			let cardinal = CachedTaggedTrapoSet.cardinal in
			let image = SimpleCachedTaggedTrapo.image meta_internal internal_cached_tagged_trapo internal_cached_trapo in
			let decoration = SimpleCachedTaggedTrapo.decoration internal_cached_tagged_trapo in
			let format_word = SimpleCachedTaggedTrapo.format_word meta_internal internal_cached_trapo internal_cached_tagged_trapo in
			let iter_sub = CachedTaggedTrapoSet.iter_sub in
			let empty = CachedTaggedTrapoSet.empty meta_internal in
			let is_empty = CachedTaggedTrapoSet.is_empty in
			let singles_internal = CachedTaggedTrapoSet.singles meta_internal internal_cached_trapo internal_cached_tagged_trapo in
			(*
			let compose_internal_internal = TaggedTrapoComposers.compose_internal_internal meta_internal in
			let join_internal_internal = TaggedTrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
			let union = TaggedTrapoSet.union in
			let diff = TaggedTrapoSet.diff in
			let cardinal = TaggedTrapoSet.cardinal in
			let image = SimpleTaggedTrapo.image meta_internal in
			let decoration = SimpleTaggedTrapo.decoration in
			let format_word = SimpleTaggedTrapo.format_word meta_internal in
			let iter_sub = TaggedTrapoSet.iter_sub in
			let empty = TaggedTrapoSet.empty meta_internal in
			let is_empty = TaggedTrapoSet.is_empty in
			let singles_internal = TaggedTrapoSet.singles meta_internal in
			*)

			let new_internal = ref singles_internal in
			let boxes_internal = ref empty in
    	let bcount = ref (cardinal !new_internal) in
    
			let subsumed = ref None in

    	while (!subsumed = None) && not (is_empty !new_internal) do
        iter_sub (fun m ->
      	  let (_, _, s) = decoration m in
					if (!subsumed = None) && (NBA.accept nba2 s) then (
						let img = image m (Iterators.singleton (MetaTrapo.initial (MetaTaggedTrapo.left meta_internal))) in
						if not (TreeSet.exists (NBA.accept nba1) img)
						then subsumed := Some (format_word m);
					)
				) ((MetaTrapo.initial (MetaTaggedTrapo.right meta_internal)), ()) !new_internal;
				if !subsumed = None then (
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
			({box_count = !bcount}, !subsumed)			

end;;








module NpvpaSubsumption = struct

	type subsumption_info = {
			box_count: int;
		}
		
  let is_subsumed npvpa2 npvpa1 iter2 iter1 alphaiter stack2 stack1 =
		let (meta_internal, meta_ret, meta_call) = NpvpaMetaTaggedTrapo.make npvpa1 iter1 npvpa2 iter2 alphaiter stack1 stack2 in

		let ((internal_cached_trapo, internal_cached_tagged_trapo) as internal_cache) = TaggedTrapoCaches.internal_caches meta_internal in
		let ((return_cached_trapo, return_cached_tagged_trapo) as return_cache) = TaggedTrapoCaches.return_caches meta_ret in
		let ((call_cached_trapo, call_cached_tagged_trapo) as call_cache) = TaggedTrapoCaches.call_caches meta_call in
		let compose_internal_internal = TaggedTrapoComposers.compose_cached_internal_internal_cached meta_internal internal_cache in
		let compose_call_return = TaggedTrapoComposers.compose_cached_call_return_cached meta_call meta_ret call_cache return_cache internal_cache in
		let compose_call_internal = TaggedTrapoComposers.compose_cached_call_internal_cached meta_call meta_internal call_cache internal_cache in
		let join_internal_internal = TaggedTrapoJoiners.join_internal_internal_cached meta_internal internal_cache compose_internal_internal in
		let join_call_return = TaggedTrapoJoiners.join_call_return_cached meta_internal meta_ret call_cache return_cache internal_cache compose_call_return in
		let join_call_internal = TaggedTrapoJoiners.join_call_internal_cached meta_call meta_internal call_cache internal_cache compose_call_internal in
		let union = CachedTaggedTrapoSet.union in
		let diff = CachedTaggedTrapoSet.diff in
		let cardinal = CachedTaggedTrapoSet.cardinal in
		let fold = CachedTaggedTrapoSet.fold in
		let image = SimpleCachedTaggedTrapo.image meta_internal internal_cached_tagged_trapo internal_cached_trapo in
    let preimage = SimpleCachedTaggedTrapo.preimage meta_internal internal_cached_tagged_trapo internal_cached_trapo in
		let decoration = SimpleCachedTaggedTrapo.decoration internal_cached_tagged_trapo in
		let get_word = SimpleCachedTaggedTrapo.get_word internal_cached_tagged_trapo in
		let fold_sub = CachedTaggedTrapoSet.fold_sub in
		let is_idempotent = SimpleCachedTaggedTrapo.is_idempotent meta_internal internal_cached_trapo internal_cached_tagged_trapo in
		let empty = CachedTaggedTrapoSet.empty meta_internal in
		let good_states = SimpleCachedTaggedTrapo.good_states meta_internal internal_cached_tagged_trapo internal_cached_trapo in
		let is_empty = CachedTaggedTrapoSet.is_empty in
		let singles_call = CachedTaggedTrapoSet.singles meta_call call_cached_trapo call_cached_tagged_trapo in
		let singles_ret = CachedTaggedTrapoSet.singles meta_ret return_cached_trapo return_cached_tagged_trapo in
		let singles_internal = CachedTaggedTrapoSet.singles meta_internal internal_cached_trapo internal_cached_tagged_trapo in
		let add = CachedTaggedTrapoSet.add internal_cached_tagged_trapo in
		let collapse_right = CachedTaggedTrapo.collapse_right meta_call meta_internal call_cached_trapo call_cached_tagged_trapo internal_cached_trapo internal_cached_tagged_trapo in
		let project_left = CachedTaggedTrapo.project_left meta_ret return_cached_trapo return_cached_tagged_trapo internal_cached_trapo internal_cached_tagged_trapo in
		
		(*
		let compose_internal_internal = TaggedTrapoComposers.compose_internal_internal meta_internal in
		let compose_call_return = TaggedTrapoComposers.compose_call_return meta_call meta_ret in
		let compose_call_internal = TaggedTrapoComposers.compose_call_internal meta_call meta_internal in
		let join_internal_internal = TaggedTrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
		let join_call_return = TaggedTrapoJoiners.join_call_return meta_internal meta_ret compose_call_return in
		let join_call_internal = TaggedTrapoJoiners.join_call_internal meta_call meta_internal compose_call_internal in
		let union = TaggedTrapoSet.union in
		let diff = TaggedTrapoSet.diff in
		let cardinal = TaggedTrapoSet.cardinal in
		let image = SimpleTaggedTrapo.image meta_internal in
    let preimage = SimpleTaggedTrapo.preimage meta_internal in
		let decoration = SimpleTaggedTrapo.decoration in
		let get_word = SimpleTaggedTrapo.get_word in
		let fold_sub = TaggedTrapoSet.fold_sub in
		let fold = TaggedTrapoSet.fold in
		let is_idempotent = SimpleTaggedTrapo.is_idempotent meta_internal in
		let empty = TaggedTrapoSet.empty meta_internal in
		let good_states = SimpleTaggedTrapo.good_states meta_internal in
		let is_empty = TaggedTrapoSet.is_empty in
		let singles_call = TaggedTrapoSet.singles meta_call in
		let singles_ret = TaggedTrapoSet.singles meta_ret in
		let singles_internal = TaggedTrapoSet.singles meta_internal in
		let add = TaggedTrapoSet.add meta_internal in
		let collapse_right = TaggedTrapo.collapse_right meta_call meta_internal in
		let project_left = TaggedTrapo.project_left meta_ret in
		*)


		let singles_internal = union (join_call_return singles_call singles_ret) singles_internal in

		let new_internal = ref singles_internal in
		let boxes_internal = ref empty in
	  let bcount = ref (cardinal !new_internal) in

		let subsumed = ref None in

		let idempots = ref (TaggedStatesetSet.empty meta_internal) in
		let reachers = ref (TaggedStatesetSet.epsilon meta_internal) in
		
		let update_and_check_subsumed set =
			let new_idempots = fold (fun m i ->
				let (r, p, _) = decoration m in
				if (p mod 2 = 0) && (is_idempotent m) then (
					let good = preimage m (good_states m) in
					if (TaggedStatesetSet.subsumed i r good) || (TaggedStatesetSet.subsumed !idempots r good) then i
					else TaggedStatesetSet.add meta_internal i r good (get_word m)
				)
				else i
			) set (TaggedStatesetSet.empty meta_internal) in
			subsumed := TaggedStatesetSet.reacher_idempot_match meta_internal !reachers new_idempots;
			if !subsumed = None then (
				idempots := TaggedStatesetSet.union !idempots new_idempots;
				let new_reachers = fold_sub (fun m r ->
					let (_,_,q) = decoration m in
					let image = image m (Iterators.singleton (MetaTrapo.initial (MetaTaggedTrapo.left meta_internal))) in
					if (TaggedStatesetSet.subsumed r q image) ||
					   (TaggedStatesetSet.subsumed !reachers q image) then r
					else TaggedStatesetSet.add meta_internal r q image (get_word m)
				) ((MetaTrapo.initial (MetaTaggedTrapo.right meta_internal)), ()) set (TaggedStatesetSet.empty meta_internal) in
				subsumed := TaggedStatesetSet.reacher_idempot_match meta_internal new_reachers !idempots;
				if !subsumed = None then reachers := TaggedStatesetSet.union !reachers new_reachers
			)
		in
		
    while (!subsumed = None) && not (is_empty !new_internal) do
			update_and_check_subsumed !new_internal;
			if !subsumed = None then (
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
		if (!subsumed = None) && !check_call_collapsed then (
			let singles_call_collapsed = fold (fun trapo -> add (collapse_right trapo)) singles_call empty in
			let new_call_collapsed = ref singles_call_collapsed in
			let call_collapsed = ref !boxes_internal in
			while (!subsumed = None) && not (is_empty !new_call_collapsed) do
			  update_and_check_subsumed !new_call_collapsed;
				if !subsumed = None then (
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
		if (!subsumed = None) && !check_ret_projected then (
  		let singles_ret_projected = fold (fun trapo acc -> match project_left trapo None None with None -> acc | Some i -> add i acc) singles_ret empty in
			let new_ret_projected = ref singles_ret_projected in
			let ret_projected = ref !boxes_internal in
			while (!subsumed = None) && not (is_empty !new_ret_projected) do
			  update_and_check_subsumed !new_ret_projected;
				if !subsumed = None then (
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
		({ box_count = !bcount}, !subsumed)

end;;




module FiniteNbvpaSubsumption = struct
	
	type subsumption_info = {
		box_count: int;
	}
		
 	let is_subsumed nbvpa2 nbvpa1 iter2 iter1 alphaiter stack2 stack1 =
		(* Special case: epsilon *)
		if (NBVPA.accept nbvpa2 (NMVPA.initial nbvpa2)) && not (NBVPA.accept nbvpa1 (NMVPA.initial nbvpa1))
		then ({box_count = 0}, Some "")
		else
			let (meta_internal, meta_ret, meta_call) = FiniteNbvpaMetaTaggedTrapo.make nbvpa1 iter1 nbvpa2 iter2 alphaiter stack1 stack2 in
						
			let ((internal_cached_trapo, internal_cached_tagged_trapo) as internal_cache) = TaggedTrapoCaches.internal_caches meta_internal in
			let ((return_cached_trapo, return_cached_tagged_trapo) as return_cache) = TaggedTrapoCaches.return_caches meta_ret in
			let ((call_cached_trapo, call_cached_tagged_trapo) as call_cache) = TaggedTrapoCaches.call_caches meta_call in
			let compose_internal_internal = TaggedTrapoComposers.compose_cached_internal_internal_cached meta_internal internal_cache in
			let compose_call_return = TaggedTrapoComposers.compose_cached_call_return_cached meta_call meta_ret call_cache return_cache internal_cache in
			let compose_call_internal = TaggedTrapoComposers.compose_cached_call_internal_cached meta_call meta_internal call_cache internal_cache in
			let join_internal_internal = TaggedTrapoJoiners.join_internal_internal_cached meta_internal internal_cache compose_internal_internal	in
			let join_call_return = TaggedTrapoJoiners.join_call_return_cached meta_internal meta_ret call_cache return_cache internal_cache compose_call_return in
			let join_call_internal = TaggedTrapoJoiners.join_call_internal_cached meta_call meta_internal call_cache internal_cache compose_call_internal in
			let union = CachedTaggedTrapoSet.union in
			let diff = CachedTaggedTrapoSet.diff in
			let cardinal = CachedTaggedTrapoSet.cardinal in
			let image = SimpleCachedTaggedTrapo.image meta_internal internal_cached_tagged_trapo internal_cached_trapo in
			let decoration = SimpleCachedTaggedTrapo.decoration internal_cached_tagged_trapo in
			let format_word = SimpleCachedTaggedTrapo.format_word meta_internal internal_cached_trapo internal_cached_tagged_trapo in
			let iter_sub = CachedTaggedTrapoSet.iter_sub in
			let empty = CachedTaggedTrapoSet.empty meta_internal in
			let is_empty = CachedTaggedTrapoSet.is_empty in
			let singles_call = CachedTaggedTrapoSet.singles meta_call call_cached_trapo call_cached_tagged_trapo in
			let singles_ret = CachedTaggedTrapoSet.singles meta_ret return_cached_trapo return_cached_tagged_trapo in
			let singles_internal = CachedTaggedTrapoSet.singles meta_internal internal_cached_trapo internal_cached_tagged_trapo in
			let fold = CachedTaggedTrapoSet.fold in
			let add = CachedTaggedTrapoSet.add internal_cached_tagged_trapo in
			let collapse_right = CachedTaggedTrapo.collapse_right meta_call meta_internal call_cached_trapo call_cached_tagged_trapo internal_cached_trapo internal_cached_tagged_trapo in
			let project_left = CachedTaggedTrapo.project_left meta_ret return_cached_trapo return_cached_tagged_trapo internal_cached_trapo internal_cached_tagged_trapo in

(*			
			let compose_internal_internal = TaggedTrapoComposers.compose_internal_internal meta_internal in
			let compose_call_return = TaggedTrapoComposers.compose_call_return meta_call meta_ret in
			let compose_call_internal = TaggedTrapoComposers.compose_call_internal meta_call meta_internal in
			let join_internal_internal = TaggedTrapoJoiners.join_internal_internal meta_internal compose_internal_internal	in
			let join_call_return = TaggedTrapoJoiners.join_call_return meta_internal meta_ret compose_call_return in
			let join_call_internal = TaggedTrapoJoiners.join_call_internal meta_call meta_internal compose_call_internal in
			let union = TaggedTrapoSet.union in
			let diff = TaggedTrapoSet.diff in
			let cardinal = TaggedTrapoSet.cardinal in
			let image = SimpleTaggedTrapo.image meta_internal in
			let decoration = SimpleTaggedTrapo.decoration in
			let format_word = SimpleTaggedTrapo.format_word meta_internal in
			let iter_sub = TaggedTrapoSet.iter_sub in
			let empty = TaggedTrapoSet.empty meta_internal in
			let is_empty = TaggedTrapoSet.is_empty in
			let singles_call = TaggedTrapoSet.singles meta_call in
			let singles_ret = TaggedTrapoSet.singles meta_ret in
			let singles_internal = TaggedTrapoSet.singles meta_internal in
			let fold = TaggedTrapoSet.fold in
			let add = TaggedTrapoSet.add meta_internal in
			let collapse_right = TaggedTrapo.collapse_right meta_call meta_internal in
			let project_left = TaggedTrapo.project_left meta_ret in
*)

			let test_subsumed set =
				let subsumed = ref None in
        iter_sub (fun m ->
      	  let (_, _, s) = decoration m in
					if (!subsumed = None) && (NBVPA.accept nbvpa2 s) then (
						let img = image m (Iterators.singleton (MetaTrapo.initial (MetaTaggedTrapo.left meta_internal))) in
						if not (TreeSet.exists (NBVPA.accept nbvpa1) img)
						then subsumed := Some (format_word m);
					)
				) ((MetaTrapo.initial (MetaTaggedTrapo.right meta_internal)), ()) set;
				!subsumed
			in

			let singles_internal = union (join_call_return singles_call singles_ret) singles_internal in
			let new_internal = ref singles_internal in
			let boxes_internal = ref empty in
    	let bcount = ref (cardinal !new_internal) in
    
			let subsumed = ref None in

    	while (!subsumed = None) && not (is_empty !new_internal) do
				subsumed := test_subsumed !new_internal;
				if !subsumed = None then (
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
			if (!subsumed = None) && !check_call_collapsed then (
				let singles_call_collapsed = fold (fun trapo -> add (collapse_right trapo)) singles_call empty in
				let new_call_collapsed = ref singles_call_collapsed in
				let call_collapsed = ref !boxes_internal in
				while (!subsumed = None) && not (is_empty !new_call_collapsed) do
				  subsumed := test_subsumed !new_call_collapsed;
					if !subsumed = None then (
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
			if (!subsumed = None) && !check_ret_projected then (
  			let singles_ret_projected = fold (fun trapo acc -> match project_left trapo None None with None -> acc | Some i -> add i acc) singles_ret empty in
				let new_ret_projected = ref singles_ret_projected in
				let ret_projected = ref !boxes_internal in
				while (!subsumed = None) && not (is_empty !new_ret_projected) do
				  subsumed := test_subsumed !new_ret_projected;
					if !subsumed = None then (
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
			({box_count = !bcount}, !subsumed)

end;;


let register _ =
SubsumptionSolvers.register {
	SubsumptionSolvers.ident = "npa";
	SubsumptionSolvers.description = "npa subsumption test";
	SubsumptionSolvers.automata_class_major = NpaClass;
	SubsumptionSolvers.automata_class_minor = NpaClass;
	SubsumptionSolvers.solve = fun x y -> (
		match (x,y) with
			(NpaType (npa1, states1, _), NpaType (npa2, states2, alpha)) -> (
					let f info _ = "Info: " ^ string_of_int (info.NpaSubsumption.box_count) ^ " morphisms\n" in
					match NpaSubsumption.is_subsumed npa1 npa2 states1 states2 alpha with
						(info, None) -> (SubsumptionSolvers.Subsumed, f info)
					|	(info, Some s) -> (SubsumptionSolvers.NotSubsumed s, f info)
			)
		| _ -> failwith "wrong automaton class"
	);
};
SubsumptionSolvers.register {
	SubsumptionSolvers.ident = "finite_nba";
	SubsumptionSolvers.description = "finite nba subsumption test";
	SubsumptionSolvers.automata_class_major = NbaClass;
	SubsumptionSolvers.automata_class_minor = NbaClass;
	SubsumptionSolvers.solve = fun x y -> (
		match (x,y) with
			(NbaType (nba1, states1, _), NbaType (nba2, states2, alpha)) -> (
					let f info _ = "Info: " ^ string_of_int (info.FiniteNbaSubsumption.box_count) ^ " morphisms\n" in
					match FiniteNbaSubsumption.is_subsumed nba1 nba2 states1 states2 alpha with
						(info, None) -> (SubsumptionSolvers.Subsumed, f info)
					|	(info, Some s) -> (SubsumptionSolvers.NotSubsumed s, f info)
			)
		| _ -> failwith "wrong automaton class"
	);
};
SubsumptionSolvers.register {
	SubsumptionSolvers.ident = "npvpa";
	SubsumptionSolvers.description = "npvpa subsumption test";
	SubsumptionSolvers.automata_class_major = NpvpaClass;
	SubsumptionSolvers.automata_class_minor = NpvpaClass;
	SubsumptionSolvers.solve = fun x y -> (
		match (x,y) with
			(NpvpaType (npvpa1, states1, _, stack1), NpvpaType (npvpa2, states2, alpha, stack2)) -> (
					let f info _ = "Info: " ^ string_of_int (info.NpvpaSubsumption.box_count) ^ " morphisms\n" in
					match NpvpaSubsumption.is_subsumed npvpa1 npvpa2 states1 states2 alpha stack1 stack2 with
						(info, None) -> (SubsumptionSolvers.Subsumed, f info)
					|	(info, Some s) -> (SubsumptionSolvers.NotSubsumed s, f info)
			)
		| _ -> failwith "wrong automaton class"
	);
};
SubsumptionSolvers.register {
	SubsumptionSolvers.ident = "finite_nbvpa";
	SubsumptionSolvers.description = "finite nbvpa subsumption test";
	SubsumptionSolvers.automata_class_major = NbvpaClass;
	SubsumptionSolvers.automata_class_minor = NbvpaClass;
	SubsumptionSolvers.solve = fun x y -> (
		match (x,y) with
			(NbvpaType (nbvpa1, states1, _, stack1), NbvpaType (nbvpa2, states2, alpha, stack2)) -> (
					let f info _ = "Info: " ^ string_of_int (info.FiniteNbvpaSubsumption.box_count) ^ " morphisms\n" in
					match FiniteNbvpaSubsumption.is_subsumed nbvpa1 nbvpa2 states1 states2 alpha stack1 stack2 with
						(info, None) -> (SubsumptionSolvers.Subsumed, f info)
					|	(info, Some s) -> (SubsumptionSolvers.NotSubsumed s, f info)
			)
		| _ -> failwith "wrong automaton class"
	);
};;

