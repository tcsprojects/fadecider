open Arg ;;
open Tcsbasedata;;
open Tcsset;;
open Tcsargs;;
open Tcsautomata;;
open Tcsautotransform;;
open Tcsautohelper;;
open Tcsautomataparser;;


module NPACollapse = struct

	let collapse (npa: ('q, 'a) NPA.t) states_iterator =
		let (states, alphabet, initial, delta, omega) =
			(NMA.states npa, NMA.alphabet npa, NMA.initial npa, NMA.delta npa, NMA.accept npa) in
		let max_prio = ref 0 in
		let prios = ref TreeSet.empty_def in
		states_iterator (fun q ->
			let p = omega q in
			max_prio := max !max_prio p;
			prios := TreeSet.add p !prios;
			true
		);
		let prio_to_states = Array.make (!max_prio + 1) [] in
		states_iterator (fun q ->
			let p = omega q in
			prio_to_states.(p) <- q::prio_to_states.(p);
			true
		);
		
		let (states': int Domain.t) = Domain.make compare string_of_int in
		let (alphabet': 'a Domain.t) = alphabet in
		let (initial': int) = omega initial in
		let delta' p a = Iterators.map omega (Iterators.flatten (Iterators.of_list (List.map (fun q -> delta q a) prio_to_states.(p)))) in
		let omega' (p: int) = p in
		NPA.build states' alphabet' initial' delta' omega'
		
end;;


let to_npa = function
	NpaType (auto, state_iter, alpha_iter) ->
		(auto, state_iter, alpha_iter)
|	NbaType (auto, state_iter, alpha_iter) ->
		(NBAtoNPA.transform auto, state_iter, alpha_iter)
|	DpaType (auto, state_iter, alpha_iter) ->
		(DPAtoNPA.transform auto, state_iter, alpha_iter)
|	DbaType (auto, state_iter, alpha_iter) ->
		(DBAtoNPA.transform auto, state_iter, alpha_iter)
| _ -> failwith "unknown automaton type"


module CommandLine = struct
  let speclist = []

  let header = "NPA Priority Collapse"
end ;;

open CommandLine;;

let _ =
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "\n");
  
  let (auto, states, alpha) = to_npa (parse_automaton2 stdin) in
  
  let auto = NPACollapse.collapse auto states in 
  
  let output = NMAFormatter.npa_to_string auto alpha in
  
  print_string output;
  flush stdout
