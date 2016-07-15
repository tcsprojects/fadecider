open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsset;;
open Tcsstrings;;
open Tcslist;;
open Tcstiming;;
open Tcsargs;;
open Arg;;
open Solvers;;
open Base;;


module CommandLine = struct

	let header =
		"\nRamsey-based Universality and Subsumption Checker for Automata\n" ^
		"Version 0.4, Copyright (c) 2011-2016\n\n" ^
		"Authors: Oliver Friedmann (University of Munich)\n" ^
	    "         Felix Klaedtke (ETH Zurich)\n" ^
	    "         Martin Lange (University of Kassel)\n" ^
		"\n"

	type action_type = NoAction | CheckUniversality of UniversalitySolvers.solver | CheckSubsumption of SubsumptionSolvers.solver

	let action = ref NoAction
	
	let automata_second = ref None
	
	let speclist = [
		(["-u"; "--universality"], String (fun ident -> action := CheckUniversality (UniversalitySolvers.find ident)),
          "<solver>\n     universality test, available solvers are:\n" ^
          	              UniversalitySolvers.fold (fun solver t -> t ^ "       " ^ solver.UniversalitySolvers.ident ^ " (" ^ format_automata_class solver.UniversalitySolvers.automata_class ^ ") : " ^ solver.UniversalitySolvers.description ^ "\n") "");
    (["-s"; "--subsumption"], String (fun ident -> action := CheckSubsumption (SubsumptionSolvers.find ident)),
          "<solver>\n     subsumption test, available solvers are:\n" ^
          	              SubsumptionSolvers.fold (fun solver t -> t ^ "       " ^ solver.SubsumptionSolvers.ident ^ " (" ^ format_automata_class solver.SubsumptionSolvers.automata_class_major ^ "/" ^ format_automata_class solver.SubsumptionSolvers.automata_class_minor ^ ") : " ^ solver.SubsumptionSolvers.description ^ "\n") "");
		(["-a"; "--automaton"], String (fun auto_file -> automata_second := Some (parse_automaton2 (open_in auto_file))),
		  "<file>\n     add second automaton for subsumption test");
		(["-pc"; "--pendingcalls"], Unit (fun _ -> check_call_collapsed := true),
		  "\n     allow pending calls");
		(["-pr"; "--pendingreturns"], Unit (fun _ -> check_ret_projected := true),
		  "\n     allow pending returns");
	]
	                 
end;;


let out s =
	print_string s;
	flush stdout
	
let get_automaton_info = function
	NpaType (_,s,a) -> [("type", "NPA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a))]
|	NbaType (_,s,a) -> [("type", "NBA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a))]
|	DpaType (_,s,a) -> [("type", "DPA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a))]
|	DbaType (_,s,a) -> [("type", "DBA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a))]
|	NpvpaType (_,s,a,st) -> [("type", "NPVPA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a)); ("stack", string_of_int (Iterators.cardinal st))]
|	NbvpaType (_,s,a,st) -> [("type", "NBVPA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a)); ("stack", string_of_int (Iterators.cardinal st))]
|	DpvpaType (_,s,a,st) -> [("type", "DPVPA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a)); ("stack", string_of_int (Iterators.cardinal st))]
|	DbvpaType (_,s,a,st) -> [("type", "DBVPA"); ("states", string_of_int (Iterators.cardinal s)); ("alphabet", string_of_int (Iterators.cardinal a)); ("stack", string_of_int (Iterators.cardinal st))]

let format_automaton_info info =
	ListUtils.format (fun (x,y) -> x ^ ":" ^ y) info

open CommandLine;;

let _ =
	SimpleArgs.parsedef speclist (fun _ -> failwith "no custom parameters required / supported")
	                             (header ^ "Usage: cat automaton1-file | fadecider (-u|-s) [-a automaton2-file]\n" ^ "\nOptions are as follows:");
	                             
	let automata_first = parse_automaton2 stdin in
	
	out header;
	
	out ("Automaton 1: " ^ format_automaton_info (get_automaton_info automata_first) ^ "\n"); 
	
	(
	match !action with
		NoAction -> (
			out "No action selected.\n"
		)
	|	CheckUniversality solver -> (
			if !automata_second != None then out "Ignoring second automaton.\n";
			out "\n";
			out ("Solver: " ^ solver.UniversalitySolvers.ident ^ "\n");
			if not (automata_class_match automata_first solver.UniversalitySolvers.automata_class) then failwith "Automata class mismatch!\n";
			out "Solving... ";
			let t = SimpleTiming.init true in
			let (result, info) = solver.UniversalitySolvers.solve automata_first in
			out ("finished, t = " ^ SimpleTiming.format t ^ "\n\n");
			out (info ());
			match result with
				UniversalitySolvers.Universal -> out "Automaton is universal.\n"
			|	UniversalitySolvers.NotAccepting s -> out ("Automaton does not accept '" ^ s ^ "'.\n")
		)
	|	CheckSubsumption solver -> (
			if !automata_second = None then failwith "Requiring second automaton to check for subsumption!\n";
			let automata_second = OptionUtils.get_some !automata_second in
			out ("Automaton 2: " ^ format_automaton_info (get_automaton_info automata_second) ^ "\n"); 
			out "\n";
			out ("Solver: " ^ solver.SubsumptionSolvers.ident ^ "\n");
			if not (automata_class_match automata_first solver.SubsumptionSolvers.automata_class_major) then failwith "Automata class mismatch!\n";
			if not (automata_class_match automata_second solver.SubsumptionSolvers.automata_class_minor) then failwith "Automata class mismatch!\n";
			out "Solving... ";
			let t = SimpleTiming.init true in
			let (result, info) = solver.SubsumptionSolvers.solve automata_first automata_second in
			out ("finished, t = " ^ SimpleTiming.format t ^ "\n\n");
			out (info ());
			match result with
				SubsumptionSolvers.Subsumed -> out "Automaton is subsumed.\n"
			|	SubsumptionSolvers.NotSubsumed s -> out ("Automaton is not subsumed because of '" ^ s ^ "'.\n")
		)
	);
	
	out "\n\n";
