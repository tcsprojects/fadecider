open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsautohelper;;
open Tcsset;;
open Tcslist;;
open Tcstiming;;
open Trapo;;


module CachedTrapo : sig
	
	type ('q, 'a, 'x, 'y) s
	
	val make: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s
	
	val encode: ('q, 'a, 'x, 'y) s -> ('q, 'a, 'x, 'y) Trapo.t -> int
	
	val decode: ('q, 'a, 'x, 'y) s -> int -> ('q, 'a, 'x, 'y) Trapo.t

	val format_transitions: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> string
	
	val format_word: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> string 
	
	val format: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> string

	val get_word: ('q, 'a, 'x, 'y) s -> int -> 'a Alphabet.word
	
	val compare_words: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> int -> int
	
	val compare_transitions: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> int -> int
	
	val compare_full: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> int -> int

	val compare: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> int -> int

	val single: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> 'a -> int

	val is_idempotent: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'x) s -> int -> bool

	val image: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> ('q * 'x) Iterators.iterator -> ('q * 'y) TreeSet.t

	val preimage: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> int -> ('q * 'y) TreeSet.t -> ('q * 'x) TreeSet.t
	
	val compose: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'yy, 'z) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> ('q, 'a, 'yy, 'z) s -> ('q, 'a, 'x, 'z) s -> ('y -> 'yy) -> int -> int -> int
	
	val compose_left: ('q, 'a, unit, unit) MetaTrapo.t -> ('q, 'a, 'y, 'z) MetaTrapo.t -> ('q, 'a, unit, unit) s -> ('q, 'a, 'y, 'z) s -> int -> int -> int
	
	val compose_right: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, unit, unit) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> ('q, 'a, unit, unit) s -> int -> int -> int
	
	val collapse_right: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, unit) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> ('q, 'a, 'x, unit) s -> int -> int
	
	val project_left: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) s -> ('q, 'a, unit, 'y) s -> int -> 'x -> int

end


module CachedTrapoComposer : sig
	
	type t
	
	val empty: unit -> t
	
	val compose: t -> ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'yy, 'z) MetaTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('q, 'a, 'yy, 'z) CachedTrapo.s -> ('q, 'a, 'x, 'z) CachedTrapo.s -> ('y -> 'yy) -> int -> int -> int
	
	val compose_left: t -> ('q, 'a, unit, unit) MetaTrapo.t -> ('q, 'a, 'y, 'z) MetaTrapo.t -> ('q, 'a, unit, unit) CachedTrapo.s -> ('q, 'a, 'y, 'z) CachedTrapo.s -> int -> int -> int
	
	val compose_right: t -> ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, unit, unit) MetaTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('q, 'a, unit, unit) CachedTrapo.s -> int -> int -> int
	
end	
	

module CachedTrapoSet : sig

	type t
	
	val format: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> t -> string

	val empty: t
	
	val mem: int -> t -> bool

	val is_empty: t -> bool

	val add: int -> t -> t
	
	val singles: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> t
	
	val fold: (int -> 'b -> 'b) -> t -> 'b -> 'b
	
	val iter: (int -> unit) -> t -> unit

	val union: t -> t -> t

	val diff: t -> t -> t
	
	val cardinal: t -> int
	
	val join: (int -> int -> int) -> t -> t -> t
	
	val conditional_join: (int -> int -> int) -> t -> t -> (int -> bool) -> (int -> bool) -> t

end


module SimpleCachedTrapo : sig

	type ('q, 'a) s = ('q, 'a, unit, unit) CachedTrapo.s
		
	val make: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s

	val format_transitions: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> string 
	
	val format_word: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> string
	
	val format: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> string

	val get_word: ('q, 'a) s -> int -> 'a Alphabet.word
		
	val compare_words: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> int -> int
	
	val compare_transitions: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> int -> int
	
	val compare_full: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> int -> int

	val compare: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> int -> int

	val single: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> 'a -> int

	val is_idempotent: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> bool

	val image: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> 'q Iterators.iterator -> 'q TreeSet.t

	val preimage: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> 'q TreeSet.t -> 'q TreeSet.t
	
	val epsilon: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int

	val good_states: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) s -> int -> 'q TreeSet.t

end	
