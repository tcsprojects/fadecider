open Tcsautomataparser;;
open Tcsset;;


type automata_class = NbaClass | DbaClass | NpaClass | DpaClass | NbvpaClass | DbvpaClass | NpvpaClass | DpvpaClass

let format_automata_class = function
	NbaClass -> "NBA"
|	DbaClass -> "DBA"
|	NpaClass -> "NPA"
|	DpaClass -> "DPA"
|	NbvpaClass -> "NBVPA"
|	DbvpaClass -> "DBVPA"
|	NpvpaClass -> "NPVPA"
|	DpvpaClass -> "DPVPA"

let automata_class_match auto_type auto_class =  match (auto_type, auto_class) with
	(NbaType _, NbaClass) -> true
|	(DbaType _, DbaClass) -> true	
|	(NpaType _, NpaClass) -> true	
|	(DpaType _, DpaClass) -> true	
|	(NbvpaType _, NbvpaClass) -> true
|	(DbvpaType _, DbvpaClass) -> true	
|	(NpvpaType _, NpvpaClass) -> true	
|	(DpvpaType _, DpvpaClass) -> true	
|	_ -> false


module UniversalitySolvers = struct

	type result = Universal | NotAccepting of string
	
	type solver_function = automata_type -> result * (unit -> string)
	
	type solver = {
		ident: string;
		description: string;
		automata_class: automata_class;
		solve: solver_function;
	}
	
	let solvers = ref TreeMap.empty_def
	
	let register solver =
		if TreeMap.mem solver.ident !solvers
		then failwith ("solver `" ^ solver.ident ^ "' already registered!\n")
		else solvers := TreeMap.add solver.ident solver !solvers
	
	let mem ident = TreeMap.mem ident !solvers
	
	let find ident = TreeMap.find ident !solvers
	
	let enum it = TreeMap.iter (fun _ -> it) !solvers
	
	let fold fo b = TreeMap.fold (fun _ -> fo) !solvers b
	
end


module SubsumptionSolvers = struct

	type result = Subsumed | NotSubsumed of string
	
	type solver_function = automata_type -> automata_type -> result * (unit -> string)
	
	type solver = {
		ident: string;
		description: string;
		automata_class_major: automata_class;
		automata_class_minor: automata_class;
		solve: solver_function;
	}
	
	let solvers = ref TreeMap.empty_def
	
	let register solver =
		if TreeMap.mem solver.ident !solvers
		then failwith ("solver `" ^ solver.ident ^ "' already registered!\n")
		else solvers := TreeMap.add solver.ident solver !solvers
	
	let mem ident = TreeMap.mem ident !solvers
	
	let find ident = TreeMap.find ident !solvers
	
	let enum it = TreeMap.iter (fun _ -> it) !solvers
	
	let fold fo b = TreeMap.fold (fun _ -> fo) !solvers b
	
end
