open Tcsautomataparser;;


type automata_class = NbaClass | DbaClass | NpaClass | DpaClass | NbvpaClass | DbvpaClass | NpvpaClass | DpvpaClass

val format_automata_class: automata_class -> string

val automata_class_match: automata_type -> automata_class -> bool


module UniversalitySolvers: sig

	type result = Universal | NotAccepting of string
	
	type solver_function = automata_type -> result * (unit -> string)
	
	type solver = {
		ident: string;
		description: string;
		automata_class: automata_class;
		solve: solver_function;
	}
	
	val register: solver -> unit

	val mem: string -> bool

	val find: string -> solver
	
	val enum: (solver -> unit) -> unit
	
	val fold: (solver -> 'a -> 'a) -> 'a -> 'a
	
end


module SubsumptionSolvers: sig

	type result = Subsumed | NotSubsumed of string
	
	type solver_function = automata_type -> automata_type -> result * (unit -> string)
	
	type solver = {
		ident: string;
		description: string;
		automata_class_major: automata_class;
		automata_class_minor: automata_class;
		solve: solver_function;
	}
	
	val register: solver -> unit

	val mem: string -> bool

	val find: string -> solver
	
	val enum: (solver -> unit) -> unit
	
	val fold: (solver -> 'a -> 'a) -> 'a -> 'a
	
end
