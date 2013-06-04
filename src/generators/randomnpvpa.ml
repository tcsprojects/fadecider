open Arg ;;
open Tcsbasedata;;
open Tcsargs;;
open Tcsautomata;;
open Tcsstrings;;
open Tcsautohelper;;


module CommandLine = struct
  let states = ref 10
  let stack = ref None
  let alphabet = ref 3
  let half_priorities = ref 5
  let transition_probab = ref None
  let evenprio_probab = ref 0.5
  let buechi = ref false 
(*  let prio_list = ref None *)
  

  let speclist = [
	(["--states"; "-s"], Int (fun i -> states := i),
	  "\n     [number] of states (default = " ^ string_of_int !states ^ ")");
	(["--stack"; "-st"], Int (fun i -> stack := Some i),
	  "\n     [number] of stacks (default = states)");
	(["--alphabet"; "-a"], Int (fun i -> alphabet := i),
	  "\n     [number] of alphabet symbols (default = " ^ string_of_int !alphabet ^ ")");
	(["--prios"; "-p"], Int (fun i -> half_priorities := i),
	  "\n     [number] of even/odd priority pairs (default = " ^ string_of_int !half_priorities ^ ")");
(*	(["--priolist"; "-pl"], String (fun s -> prio_list := Some ((List.map float_of_string (StringUtils.explode s ',')))),
	  "\n     [0.5,0.25,0.1,etc.] list of prios (optional)"); *)
	(["--buechi"; "-b"], Unit (fun _ -> buechi := true),
	  "\n     make buechi automaton");
	(["--transition"; "-t"], Float (fun i -> transition_probab := Some i),
	  "\n     [probability] of transitioning between states (default = 2/states)");
	(["--evenprio"; "-e"], Float (fun i -> evenprio_probab := i),
	  "\n     [probability] of even priority (default = " ^ Printf.sprintf "%.2f" !evenprio_probab ^ ")");
  ]

  let header = "Random NPVPA Generator"
  let usage = "Usage: randomnpvpa -s [number] -a [number] -p [number] -t [probab] -e [probab] [-b]"
end ;;


let random_npvpa states alphabet stack half_priorities transition_probab evenprio_probab (*prio_list*) =
	let state_iter = Iterators.of_array (Array.init states (fun q -> q)) in
	let stack_iter = Iterators.of_array (Array.init stack (fun q -> q)) in
	let alpha_iter = (Iterators.of_array (Array.init alphabet (fun i -> 
		match (i mod 3) with
			0 -> NMVPA.Internal i
		|	1 -> NMVPA.Push i
		|	_ -> NMVPA.Pop i
	))) in
	
	let prio q = 
	(*
		match prio_list with
			None ->
			*)
				let p = 2 * Random.int half_priorities + 1 in
				if Random.float 1.0 <= evenprio_probab
				then p + 1
				else p
				(*
		|	Some l -> (
				let f = Random.float (List.fold_left + 0.0 l) in
				let i = ref 0 in
				let l = Array.of_list !l in
				while (f > 0.0) do
					
			)
			*)
	in
	
	let delta_int _ _ = 
		let l = ref [] in
		for r = 0 to states - 1 do
			if Random.float 1.0 <= transition_probab then l := r::!l
		done;
		Iterators.of_list !l
	in
	
	let delta_push _ _ =
		let l = ref [] in
		for r = 0 to states - 1 do
			for s = 0 to stack - 1 do
				if Random.float 1.0 <= transition_probab  then l := (r,s)::!l
			done
		done;
		Iterators.of_list !l
	in

	let delta_pop _ _ _ = 
		let l = ref [] in
		for r = 0 to states - 1 do
			if Random.float 1.0 <= transition_probab then l := r::!l
		done;
		Iterators.of_list !l
	in
	
	let auto =
		NPVPA.build
		(Domain.make compare string_of_int)
		(Domain.make compare (function NMVPA.Internal i -> string_of_int i | NMVPA.Push i -> string_of_int i | NMVPA.Pop i -> string_of_int i))
		(Domain.make compare string_of_int)
		0
		delta_int delta_push delta_pop
		prio
	in
	
	(auto, state_iter, alpha_iter, stack_iter)

	
open CommandLine ;;

let _ =
  Random.self_init ();
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "\n" ^ usage ^ "\n");
  if !transition_probab = None then transition_probab := Some (2.0 /. (float !states));
  if !stack = None then stack := Some !states;
  
  let (auto, states, alpha, stack) = random_npvpa !states !alphabet (OptionUtils.get_some !stack) !half_priorities (OptionUtils.get_some !transition_probab) !evenprio_probab in
  
  let formatted = 
	if !buechi
	then NMVPAFormatter.nbvpa_to_string2 (NBVPA.byNPVPA auto (fun q -> q mod 2 = 0)) states alpha stack
	else NMVPAFormatter.npvpa_to_string2 auto states alpha stack
  in
  
  print_string formatted; 
  flush stdout
  