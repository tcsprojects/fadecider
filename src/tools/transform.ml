open Arg ;;
open Tcsbasedata;;
open Tcsset;;
open Tcsargs;;
open Tcsautomata;;
open Tcsautotransform;;
open Tcsautohelper;;
open Tcsautomataparser;;


module CommandLine = struct
  let speclist = []

  let header = "Automata Transformer"
  let usage = "Usage: transform npa|nba|dpa|dba"
end ;;

let evenprios omega iter =
	TreeSet.filter (fun p -> p mod 2 = 0) (Iterators.fold iter (fun s -> TreeSet.add (omega s)) TreeSet.empty_def)

open CommandLine;;

type target_type = TargetNba | TargetNpa | TargetDba | TargetDpa

let _ =
  let target = ref "" in
  SimpleArgs.parsedef speclist (fun s -> target := s) (header ^ "\n" ^ usage ^ "\n");
  target := String.lowercase !target;  
  let target_type = 
	  if !target = "nba" then TargetNba
	  else if !target = "npa" then TargetNpa
	  else if !target = "dpa" then TargetDpa
	  else if !target = "dba" then TargetDba
	  else failwith ("Unknown type: '" ^ !target ^ "'")
  in

  let source = parse_automaton2 stdin in

  let output = match source with
      NpaType (auto, state_iter, alpha_iter) -> (match target_type with
          TargetNpa -> NMAFormatter.npa_to_string2 auto state_iter alpha_iter
      |   TargetNba -> NMAFormatter.nba_to_string (NPAtoNBA.transform auto (evenprios (NPA.omega auto) state_iter)) alpha_iter
      |   TargetDpa -> DMAFormatter.dpa_to_string (NPAtoDPA.transform auto (evenprios (NPA.omega auto) state_iter) (Iterators.cardinal state_iter)) alpha_iter
	  |   _ -> failwith "impossible"
  )
  |   NbaType (auto, state_iter, alpha_iter) -> (match target_type with
          TargetNpa -> NMAFormatter.npa_to_string2 (NBAtoNPA.transform auto) state_iter alpha_iter
      |   TargetNba -> NMAFormatter.nba_to_string2 auto state_iter alpha_iter
      |   TargetDpa -> DMAFormatter.dpa_to_string (NBAtoDPA.transform auto (Iterators.cardinal state_iter)) alpha_iter
	  |   _ -> failwith "impossible"
  )
  |   DpaType (auto, state_iter, alpha_iter) -> (match target_type with
          TargetNpa -> NMAFormatter.npa_to_string2 (DPAtoNPA.transform auto) state_iter alpha_iter
      |   TargetNba -> NMAFormatter.nba_to_string (DPAtoNBA.transform auto (evenprios (DPA.omega auto) state_iter)) alpha_iter
      |   TargetDpa -> DMAFormatter.dpa_to_string2 auto state_iter alpha_iter
	  |   _ -> failwith "impossible"
  )
  |   DbaType (auto, state_iter, alpha_iter) -> (match target_type with
          TargetNpa -> NMAFormatter.npa_to_string2 (DBAtoNPA.transform auto) state_iter alpha_iter
      |   TargetNba -> NMAFormatter.nba_to_string2 (DBAtoNBA.transform auto) state_iter alpha_iter
      |   TargetDpa -> DMAFormatter.dpa_to_string2 (DBAtoDPA.transform auto) state_iter alpha_iter
      |   TargetDba -> DMAFormatter.dba_to_string2 auto state_iter alpha_iter
  )
  |   _ -> failwith "impossible"
  in
  
  print_string output;
  flush stdout
