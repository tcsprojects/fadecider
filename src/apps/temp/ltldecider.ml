open Tcsformulaparse;;
open Tcsmetaformula;;
open Tcsltlformula;;
open Tcsset;;
open Tcscache;;
open Tcsbasedata;;

type box_result = Bottom | One | Two | Flash
type box = (int * int, box_result) TreeMap.t

let _ =
	(* General *)
	let fold_for f n acc = let rec helper i a = if i < n then helper (i+1) (f i a) else a in helper 0 acc in

	(* Box Comparison *)
	let cmp_rel a b = let map = function One -> 1 | Two -> 2 | Flash -> 3 | Bottom -> 4 in map a - map b in
  let max_rel a b = if cmp_rel a b >= 0 then a else b in
  let cmp_rew a b = let map = function Flash -> 1 | Bottom -> 2 | One -> 3 | Two -> 4 in map a - map b in
  let max_rew a b = if cmp_rew a b >= 0 then a else b in
			
	(* Box Functions *)
	let format_box_result = function Bottom -> "B" | One -> "1" | Two -> "2" | Flash -> "F" in
	let format_box d = TreeMap.format (Formators.product (Formators.product (format_decomposed_formula d) (format_decomposed_formula d)) format_box_result) in
	
	let box_by_function decformula f =
  	let (initial, formulas, links, props) = decformula in
		fold_for (fun i -> fold_for (fun j -> TreeMap.add (i,j) (f i j)) (Array.length formulas)) (Array.length formulas) TreeMap.empty_def
  in

	let image_at decformula f box =
		let (initial, formulas, links, props) = decformula in
		fold_for (fun i -> TreeSet.add (TreeMap.find (f, i) box)) (Array.length formulas) TreeSet.empty_def
  in

	let bottom_free_initial_image decformula box =
		let (initial, formulas, links, props) = decformula in
		fold_for (fun i a -> if TreeMap.find (initial, i) box != Bottom then TreeSet.add i a else a) (Array.length formulas) TreeSet.empty_def
	in

	let contradiction_free_at decformula f box = not (TreeSet.mem Flash (image_at decformula f box)) in
	
	let compose decformula box_a box_b =
		let (initial, formulas, links, props) = decformula in
		let f a b =
			if (not(contradiction_free_at decformula a box_b)) then Flash else (
				let result = ref Flash in
				Array.iteri (fun i _ ->
					let left = TreeMap.find (a, i) box_b in
					let right = TreeMap.find (i, b) box_a in
					result := max_rew !result (max_rel left right) 
				) formulas;
				!result
			)
		in
		  box_by_function decformula f
		in
	
	let is_idempotent decformula box = TreeMap.compare compare (compose decformula box box) box == 0 in
	(*
	let is_good_at decformula f box = contradiction_free_at decformula f box && (not (TreeSet.mem One (image_at decformula f box))) in
*)
  let good_image_of decformula box =
		let (initial, formulas, links, props) = decformula in
		let img = ref TreeSet.empty_def in
		Array.iteri (fun i _ ->
			let img_at = image_at decformula i box in
			if not (TreeSet.mem Flash img_at || TreeSet.mem One img_at) then img := TreeSet.add i !img;
		) formulas;
		!img
  in
		
	let iter_initial_formulas decformula cb =
		let add f = cb (box_by_function decformula f) in
  	let (initial, formulas, links, props) = decformula in
		Array.iteri (fun i f ->
		  match f with
			| FIntBranch (player, left, right) ->
				if (player) then (
					add (fun a b -> if ((a != i) && (a == b)) || ((a == i) && (left == b) ) then One else Bottom);
					add (fun a b -> if ((a != i) && (a == b)) || ((a == i) && (right == b) ) then One else Bottom)
			  ) else (
					add (fun a b -> if ((a != i) && (a == b)) || ((a == i) && (left == b || right == b) ) then One else Bottom)
			  )
			| FIntFixpoint (player, left, right) ->
				if (player) then (
					add (fun a b -> if ((a != i) && (a == b)) || ((a == i) && (right == b) ) then One else Bottom);
					add (fun a b -> if ((a != i) && (a == b)) || ((a == i) && (left == b || links.(i).(0) == b) ) then One else Bottom)
				) else (
					add (fun a b -> if ((a != i) && (a == b)) || ((a == i) && (left == b || right == b) ) then One else Bottom);
					add (fun a b -> if ((a != i) && (a == b)) || ((a == i) && (left == b)) then One else if ((a == i) && (links.(i).(0) == b) ) then Two else Bottom)
				)
			| FIntAtom player ->
				  add (fun a b -> if player then Two else One)
		  | _ -> ()
		) formulas;
		let prop_set = TreeSet.of_array_def (Array.init (Array.length props) (fun i -> i)) in
		let prop_sub_sets = ref [] in
		TreeSet.iterate_subsets prop_set (fun s -> prop_sub_sets := s::!prop_sub_sets);
		let prop_sub_sets = !prop_sub_sets in
		List.iter (fun set ->
			add (fun a b ->
				match formulas.(a) with
				| FIntNext i -> if i == b then One else Bottom
				| FIntProp (player, i) -> if (player && TreeSet.mem i set) || (not player &&  not(TreeSet.mem i set)) then Bottom else Flash
				| _ -> Flash
		  )
		) prop_sub_sets;
		add (fun a b -> if (initial == a) && (a == b) then One else Bottom)
  in

	(* Application *)
	let formula_string = Sys.argv.(1) in
	let meta_formula = eval_formula (parse_expression formula_string) [] in
	let ltl_formula = eval_metaformula meta_formula in
	let ltl_pos_formula = formula_to_positive ltl_formula in
	let decformula = normal_form_formula_to_decomposed_formula ltl_pos_formula ltl_formula_link_map in

	let boxes = ref (TreeSet.empty (TreeMap.compare compare)) in
	let new_boxes = ref (TreeSet.empty (TreeMap.compare compare)) in
	let images = ref (TreeMap.empty TreeSet.compare) in
	let new_images = ref (TreeMap.empty TreeSet.compare) in
	let goodidem = ref (TreeMap.empty TreeSet.compare) in
	let new_goodidem = ref (TreeMap.empty TreeSet.compare) in
	
	iter_initial_formulas decformula (fun box -> new_boxes := TreeSet.add box !new_boxes);
	
	let initial_boxes = !new_boxes in

	TreeSet.iter (fun box ->
		print_string (format_box decformula box ^ "\n");
	) initial_boxes;
	
	let found = ref None in
	
	while (!found == None && not (TreeSet.is_empty !new_boxes)) do
		TreeSet.iter (fun box ->
			let image = bottom_free_initial_image decformula box in
			print_string ("Box Image " ^ format_box decformula box ^ " is " ^ (TreeSet.format (format_decomposed_formula decformula) image) ^ "\n");
			if not (TreeMap.mem image !images) then new_images := TreeMap.add image box !new_images;
			if (is_idempotent decformula box) then (
				let goodimage = good_image_of decformula box in
  			print_string ("Idempotent Box Image " ^ format_box decformula box ^ " is " ^ (TreeSet.format (format_decomposed_formula decformula) goodimage) ^ "\n");
				if not (TreeMap.mem goodimage !goodidem) then new_goodidem := TreeMap.add goodimage box !new_goodidem;
			);
		) !new_boxes;
		let test imgs idms = 
			let found = ref None in
			TreeMap.iter (fun img (imgbox: box) ->
				TreeMap.iter (fun idm (idmbox: box) ->
					if (!found == None) && (TreeSet.subset img idm)
					then found := Some (imgbox, idmbox)
				) idms;
			) imgs;
			!found
		in
		found := test !new_images !goodidem;
		if (!found == None) then (
			found := test !images !new_goodidem;
			if (!found == None) then (
				found := test !new_images !new_goodidem;
				if (!found == None) then (
					images := TreeMap.fold TreeMap.add !images !new_images;
					new_images := TreeMap.empty TreeSet.compare;
					goodidem := TreeMap.fold TreeMap.add !goodidem !new_goodidem;
					new_goodidem := TreeMap.empty TreeSet.compare;
					let newer_boxes = ref (TreeSet.empty (TreeMap.compare compare)) in
					TreeSet.iter (fun left ->
						TreeSet.iter (fun right ->
							let composed = compose decformula left right in
			  			print_string ("Compose\n  " ^ format_box decformula left ^ "\n  " ^ (format_box decformula right) ^ "\n  " ^ (format_box decformula composed) ^ "\n");
							if not (TreeSet.mem composed !boxes || TreeSet.mem composed !new_boxes)
							then newer_boxes := TreeSet.add composed !newer_boxes
						) initial_boxes
					) !new_boxes;
					boxes := TreeSet.union !boxes !new_boxes;
					new_boxes := !newer_boxes
				);
			);
		);
	done; 
	match !found with
	| None -> print_string "unsatisfiable\n";
	| Some (head, tail) -> print_string "satisfiable\n";;
		
