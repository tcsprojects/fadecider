open Tcsautomataparser;;
open Tcsset;;


type automata_class = Solverregistry.automata_class

let format_automata_class = Solverregistry.format_automata_class

let automata_class_match = Solverregistry.automata_class_match


module UniversalitySolvers = struct

	type result = Solverregistry.UniversalitySolvers.result

	type solver_function = Solverregistry.UniversalitySolvers.solver_function

	type solver = Solverregistry.UniversalitySolvers.solver
	
	let register = Solverregistry.UniversalitySolvers.register
	
	let mem = Solverregistry.UniversalitySolvers.mem
	
	let find = Solverregistry.UniversalitySolvers.find
	
	let enum = Solverregistry.UniversalitySolvers.enum
	
	let fold = Solverregistry.UniversalitySolvers.fold
	
end


module SubsumptionSolvers = struct

	type result = Solverregistry.SubsumptionSolvers.solver_function

	type solver_function = Solverregistry.SubsumptionSolvers.solver_function

	type solver = Solverregistry.SubsumptionSolvers.solver
	
	let register = Solverregistry.SubsumptionSolvers.register

	let mem = Solverregistry.SubsumptionSolvers.mem
	
	let find = Solverregistry.SubsumptionSolvers.find
	
	let enum = Solverregistry.SubsumptionSolvers.enum
	
	let fold = Solverregistry.SubsumptionSolvers.fold
	
end


let _ =
    Subsumption.register ();
    Universality.register ();;