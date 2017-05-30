open Tcsautomataparser;;


type automata_class = Solverregistry.automata_class

val format_automata_class: automata_class -> string

val automata_class_match: automata_type -> automata_class -> bool


module UniversalitySolvers: sig

	type result = Solverregistry.UniversalitySolvers.result
	
	type solver_function = Solverregistry.UniversalitySolvers.solver_function
	
	type solver = Solverregistry.UniversalitySolvers.solver
	
	val register: solver -> unit

	val mem: string -> bool

	val find: string -> solver
	
	val enum: (solver -> unit) -> unit
	
	val fold: (solver -> 'a -> 'a) -> 'a -> 'a
	
end


module SubsumptionSolvers: sig

	type result = Solverregistry.SubsumptionSolvers.solver_function
	
	type solver_function = Solverregistry.SubsumptionSolvers.solver_function
	
	type solver = Solverregistry.SubsumptionSolvers.solver
	
	val register: solver -> unit

	val mem: string -> bool

	val find: string -> solver
	
	val enum: (solver -> unit) -> unit
	
	val fold: (solver -> 'a -> 'a) -> 'a -> 'a
	
end
