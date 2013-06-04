open Arg ;;
open Tcsbasedata;;
open Tcsset;;
open Tcsargs;;
open Tcsautomata;;
open Tcsautotransform;;
open Tcsautohelper;;
open Tcsautomataparser;;


module SubsumptionToUniversality = struct

	type ('a, 'b) t = Start
					| Accept
                    | InvalidRun of 'a
					| NotacceptingRun of int
					| Simulation of 'b

    type ('a, 'c) s = 'a * 'c * 'a

	let transform (npa1: ('a, 'c) NPA.t) (npa2: ('b, 'c) NPA.t) =
		let (states1, alphabet1, initial1, delta1, omega1) =
			(NMA.states npa1, NMA.alphabet npa1, NMA.initial npa1, NMA.delta npa1, NMA.accept npa1) in
		let (states2, initial2, delta2, omega2) = 
			(NMA.states npa2, NMA.initial npa2, NMA.delta npa2, NMA.accept npa2) in
		let (states1_compare, states1_format) =
			(Domain.compare states1, Domain.format states1) in
		let (alphabet1_compare, alphabet1_format) =
			(Domain.compare alphabet1, Domain.format alphabet1) in
		let (states2_compare, states2_format) =
			(Domain.compare states2, Domain.format states2) in
		
		let states3_compare x y =
			let f = function Start -> 0 | Accept -> 1 | InvalidRun _ -> 2
			               | NotacceptingRun _ -> 3 | Simulation _ -> 4 in
			let c = compare (f x) (f y) in
			if c = 0 then (
				match (x, y) with
					(InvalidRun q, InvalidRun q') -> states1_compare q q'
				|	(NotacceptingRun p, NotacceptingRun p') -> compare p p'
				|	(Simulation q, Simulation q') -> states2_compare q q'
				|	_ -> 0
			)
			else c
		in
		
		let states3_format = function
			Start -> "START"
		|	Accept -> "ACCEPT"
		|	InvalidRun q -> "I[" ^ states1_format q ^ "]"
		|	NotacceptingRun p -> "N[" ^ string_of_int p ^ "]"
		|	Simulation q -> "S[" ^ states2_format q ^ "]"
		in
		
		let alphabet3_compare (q1,a1,q1') (q2,a2,q2') =
			let c = states1_compare q1 q2 in
			if c = 0 then let c = alphabet1_compare a1 a2 in
			              if c = 0 then states1_compare q1' q2'
						  else c
			else c
		in
		
		let alphabet3_format (q,a,q') =
			"(" ^ states1_format q ^ "," ^ alphabet1_format a ^ "," ^ states1_format q' ^ ")"
		in
		
		let delta3 state (s,a,s') =
			match state with
				Simulation q -> Iterators.map (fun q' -> Simulation q') (delta2 q a)
			|	NotacceptingRun p -> Iterators.singleton (NotacceptingRun (omega1 s' + 1))
			|	InvalidRun s'' -> Iterators.singleton (if states1_compare s s'' = 0 then InvalidRun s' else Accept)
			|	Accept -> Iterators.singleton Accept
			|	Start -> if states1_compare s initial1 = 0
						 then Iterators.attach (Iterators.of_list [InvalidRun s'; NotacceptingRun (omega1 s' + 1)])
						                       (Iterators.map (fun q' -> Simulation q') (delta2 initial2 a))
						 else Iterators.singleton Accept
		in
		
		let omega3 = function
			NotacceptingRun p -> p
		|	Simulation q -> omega2 q
		|	InvalidRun _ -> 1
		|	Accept -> 0
		|	Start -> 0
		in
		
		NPA.build (Domain.make states3_compare states3_format)
			      (Domain.make alphabet3_compare alphabet3_format)
				  Start delta3 omega3


	let alphabet_iterator npa states_iterator alphabet_iterator f =
		Iterators.depend_product (Iterators.product states_iterator alphabet_iterator)
		                         (fun (q,a) -> (NMA.delta npa) q a)
								 (fun ((q,a),q') -> f (q,a,q'))
								 
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

  let header = "Subsumption to Universality Transformer"
  let usage = "Usage: cat majorautomaton-file | subtouniv minorautomaton-file"
end ;;

open CommandLine;;

let _ =
  let minorfile = ref "" in
  SimpleArgs.parsedef speclist (fun s -> minorfile := s) (header ^ "\n" ^ usage ^ "\n");

  let (major, major_states, major_alpha) = to_npa (parse_automaton2 (open_in !minorfile)) in
  let (minor, _, _) = to_npa (parse_automaton2 stdin) in
  
  let auto = SubsumptionToUniversality.transform major minor in
  let alpha = SubsumptionToUniversality.alphabet_iterator major major_states major_alpha in
  
  let output = NMAFormatter.npa_to_string auto alpha in
  
  print_string output;
  flush stdout
