open Arg ;;
open Tcsbasedata;;
open Tcsargs;;
open Tcsautomata;;
open Tcsautohelper;;


module CommandLine = struct
  let states = ref 10
  let alphabet = ref 2
  let half_priorities = ref 5
  let transition_probab = ref 0.5
  let evenprio_probab = ref 0.5
  

  let speclist = [
	(["--states"; "-s"], Int (fun i -> states := i),
	  "\n     [number] of states (default = " ^ string_of_int !states ^ ")");
	(["--alphabet"; "-a"], Int (fun i -> alphabet := i),
	  "\n     [number] of alphabet symbols (default = " ^ string_of_int !alphabet ^ ")");
	(["--prios"; "-p"], Int (fun i -> half_priorities := i),
	  "\n     [number] of even/odd priority pairs (default = " ^ string_of_int !half_priorities ^ ")");
	(["--transition"; "-t"], Float (fun i -> transition_probab := i),
	  "\n     [probability] of transitioning between states (default = " ^ Printf.sprintf "%.2f" !transition_probab ^ ")");
	(["--evenprio"; "-e"], Float (fun i -> evenprio_probab := i),
	  "\n     [probability] of even priority (default = " ^ Printf.sprintf "%.2f" !evenprio_probab ^ ")");
  ]

  let header = "Random NPA Generator"
  let usage = "Usage: randomnpa -s [number] -a [number] -p [number] -t [probab] -e [probab]"
end ;;


let random_npa states alphabet half_priorities transition_probab evenprio_probab =
	let prios = Array.init states (fun q ->
		let p = 2 * Random.int half_priorities + 1 in
		if Random.float 1.0 <= evenprio_probab
		then p + 1
		else p
	) in
	let trans = Array.init states (fun q ->
		Array.init alphabet (fun i ->
			let l = ref [] in
			for r = 0 to states - 1 do
				if Random.float 1.0 <= transition_probab then l := r::!l
			done;
			!l
		)
	) in
	
	let npa =
		NPA.build
		(Domain.make compare string_of_int)
		(Domain.make compare Char.escaped)
		0
		(fun q a -> Iterators.of_list trans.(q).(Char.code a - Char.code 'a'))
		(fun q -> prios.(q))
	in
	
	let alpha_iter = (Iterators.of_array (Array.init alphabet (fun i -> Char.chr (i + Char.code 'a')))) in
	
	(npa, alpha_iter)



open CommandLine ;;

let _ =
  Random.self_init ();
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "\n" ^ usage ^ "\n");
  
  let (auto, alpha) = random_npa !states !alphabet !half_priorities !transition_probab !evenprio_probab in
  
  let formatted = NMAFormatter.npa_to_string auto alpha in
  
  print_string formatted;
  flush stdout
  