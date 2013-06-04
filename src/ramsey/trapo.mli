open Tcsbasedata;;
open Tcsautomata;;
open Tcsautomataparser;;
open Tcsautohelper;;
open Tcsset;;
open Tcslist;;
open Tcstiming;;



module MetaTrapo : sig

	type ('q, 'a, 'x, 'y) t
	
	val initial: ('q, 'a, 'x, 'y) t -> 'q
	
	val states: ('q, 'a, 'x, 'y) t -> 'q Domain.t

	val source: ('q, 'a, 'x, 'y) t -> 'x Domain.t
	
	val state_source: ('q, 'a, 'x, 'y) t -> ('q * 'x) Domain.t
	
	val alphabet_iter: ('q, 'a, 'x, 'y) t -> 'a Iterators.iterator

	val state_source_iterator: ('q, 'a, 'x, 'y) t -> ('q * 'x) Iterators.iterator

	val target: ('q, 'a, 'x, 'y) t -> 'y Domain.t

	val omega: ('q, 'a, 'x, 'y) t -> ('q -> int)
	
	val omega_combine: ('q, 'a, 'x, 'y) t -> (int -> int -> int)
	
	val delta: ('q, 'a, 'x, 'y) t -> ('q * 'x) -> 'a -> ('q * 'y) Iterators.iterator
	
	val make: 'a Tcsautomata.Alphabet.alphabet ->
    'b Tcsbasedata.Domain.t ->
    'b ->
    ('b -> int) ->
    (int -> int -> int) ->
    ('b * 'c -> 'a -> ('b * 'd) Tcsbasedata.Iterators.iterator) ->
    'b Tcsbasedata.Iterators.iterator ->
    'a Tcsbasedata.Iterators.iterator ->
    'c Tcsbasedata.Domain.t ->
    'c Tcsbasedata.Iterators.iterator ->
    'd Tcsbasedata.Domain.t ->
    'd Tcsbasedata.Iterators.iterator -> ('b, 'a, 'c, 'd) t

end;;


module Trapo : sig
	
	type ('q, 'a, 'x, 'y) t
	
	
	val format_transitions: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> string 
	
	val format_word: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> string
	
	val format: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> string

	val get_word: ('q, 'a, 'x, 'y) t -> 'a Alphabet.word
	
	val set_word: ('q, 'a, 'x, 'y) t -> 'a Alphabet.word -> ('q, 'a, 'x, 'y) t
	
	val compare_words: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t -> int
	
	val compare_transitions: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t -> int
	
	val compare_full: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t -> int

	val compare: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t -> int

	val single: ('q, 'a, 'x, 'y) MetaTrapo.t -> 'a -> ('q, 'a, 'x, 'y) t

	val is_idempotent: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'x) t -> bool

	val image: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> ('q * 'x) Iterators.iterator -> ('q * 'y) TreeSet.t

	val preimage: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> ('q * 'y) TreeSet.t -> ('q * 'x) TreeSet.t
	
	val compose: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'yy, 'z) MetaTrapo.t -> ('y -> 'yy) -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'yy, 'z) t -> ('q, 'a, 'x, 'z) t

	val compose_left: ('q, 'a, unit, unit) MetaTrapo.t -> ('q, 'a, 'y, 'z) MetaTrapo.t -> ('q, 'a, unit, unit) t -> ('q, 'a, 'y, 'z) t -> ('q, 'a, 'y, 'z) t

	val compose_right: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, unit, unit) MetaTrapo.t ->  ('q, 'a, 'x, 'y) t -> ('q, 'a, unit, unit) t -> ('q, 'a, 'x, 'y) t
	
	val collapse_right: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, unit) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, unit) t
	
	val project_left: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> 'x -> ('q, 'a, unit, 'y) t

end

module TrapoSet : sig

	type ('q, 'a, 'x, 'y) t
	
	val format: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t -> string

	val empty: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t
	
	val mem: ('q, 'a, 'x, 'y) Trapo.t -> ('q, 'a, 'x, 'y) t -> bool

	val is_empty: ('q, 'a, 'x, 'y) t -> bool

	val add: ('q, 'a, 'x, 'y) Trapo.t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t

	val singles: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('q, 'a, 'x, 'y) t
	
	val fold: (('q, 'a, 'x, 'y) Trapo.t -> 'b -> 'b) -> ('q, 'a, 'x, 'y) t -> 'b -> 'b
	
	val iter: (('q, 'a, 'x, 'y) Trapo.t -> unit) -> ('q, 'a, 'x, 'y) t -> unit

	val union: ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t

	val diff: ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t -> ('q, 'a, 'x, 'y) t
	
	val cardinal: ('q, 'a, 'x, 'y) t -> int

	val join: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('qq, 'aa, 'xx, 'yy) MetaTrapo.t -> ('qqq, 'aaa, 'xxx, 'yyy) MetaTrapo.t -> 
			  (('q, 'a, 'x, 'y) Trapo.t -> ('qq, 'aa, 'xx, 'yy) Trapo.t -> ('qqq, 'aaa, 'xxx, 'yyy) Trapo.t) ->
			  ('q, 'a, 'x, 'y) t -> ('qq, 'aa, 'xx, 'yy) t -> ('qqq, 'aaa, 'xxx, 'yyy) t

	val conditional_join: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('qq, 'aa, 'xx, 'yy) MetaTrapo.t -> ('qqq, 'aaa, 'xxx, 'yyy) MetaTrapo.t -> 
			  (('q, 'a, 'x, 'y) Trapo.t -> ('qq, 'aa, 'xx, 'yy) Trapo.t -> ('qqq, 'aaa, 'xxx, 'yyy) Trapo.t) ->
			  ('q, 'a, 'x, 'y) t -> ('qq, 'aa, 'xx, 'yy) t -> (('qqq, 'aaa, 'xxx, 'yyy) Trapo.t -> bool) -> (('qqq, 'aaa, 'xxx, 'yyy) Trapo.t -> bool) -> ('qqq, 'aaa, 'xxx, 'yyy) t

end


module StatesetSet : sig

	type ('q, 'a) t
	
	val empty: ('q, 'a, _, _) MetaTrapo.t -> ('q, 'a) t
	
	val epsilon: ('q, 'a, _, _) MetaTrapo.t -> ('q, 'a) t
	
	val subsumed: ('q, 'a) t -> 'q TreeSet.t -> bool

	val add: ('q, 'a) t -> 'q TreeSet.t -> 'a Alphabet.word -> ('q, 'a) t
	
	val union: ('q, 'a) t -> ('q, 'a) t -> ('q, 'a) t
	
	val reacher_idempot_match: ('q, 'a, _, _) MetaTrapo.t -> ('q, 'a) t -> ('q, 'a) t -> string option

end


module SimpleMetaTrapo : sig

	type ('q, 'a) t = ('q, 'a, unit, unit) MetaTrapo.t
	
	val make: 'a Alphabet.alphabet -> 'q Domain.t -> 'q -> ('q -> int) -> (int -> int -> int) -> ('q -> 'a -> 'q Iterators.iterator) -> 'q Iterators.iterator -> 'a Iterators.iterator -> ('q, 'a) t

end;;


module SimpleTrapo : sig

	type ('q, 'a) t = ('q, 'a, unit, unit) Trapo.t
		
	val format_transitions: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> string 
	
	val format_word: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> string
	
	val format: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> string

	val get_word: ('q, 'a) t -> 'a Alphabet.word
	
	val compare_words: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> ('q, 'a) t -> int
	
	val compare_transitions: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> ('q, 'a) t -> int
	
	val compare_full: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> ('q, 'a) t -> int

	val compare: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> ('q, 'a) t -> int

	val single: ('q, 'a) SimpleMetaTrapo.t -> 'a -> ('q, 'a) t

	val compose: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> ('q, 'a) t -> ('q, 'a) t

	val is_idempotent: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> bool

	val image: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> 'q Iterators.iterator -> 'q TreeSet.t

	val preimage: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> 'q TreeSet.t -> 'q TreeSet.t

	val epsilon: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t

	val good_states: ('q, 'a) SimpleMetaTrapo.t -> ('q, 'a) t -> 'q TreeSet.t

end


module NpaMetaTrapo : sig

	type ('q, 'a) t = ('q, 'a) SimpleMetaTrapo.t

	val make: ('q, 'a) NPA.t -> 'q Iterators.iterator -> 'a Iterators.iterator -> ('q, 'a) t

end


module FiniteNbaMetaTrapo : sig

	type ('q, 'a) t = ('q, 'a) SimpleMetaTrapo.t

	val make: ('q, 'a) NBA.t -> 'q Iterators.iterator -> 'a Iterators.iterator -> ('q, 'a) t

end


module NpvpaMetaTrapo : sig

	type ('q, 'a) i = ('q, ('a NMVPA.nested)) SimpleMetaTrapo.t
	type ('q, 'a, 's) r = ('q, ('a NMVPA.nested), 's option, unit) MetaTrapo.t
	type ('q, 'a, 's) c = ('q, ('a NMVPA.nested), unit, 's) MetaTrapo.t
	

	val make_i: ('q, 'a, _) NPVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> ('q, 'a) i 
	val make_r: ('q, 'a, 's) NPVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> ('q, 'a, 's) r
	val make_c: ('q, 'a, 's) NPVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> ('q, 'a, 's) c
	
	val make: ('q, 'a, 's) NPVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> ('q, 'a) i * ('q, 'a, 's) r * ('q, 'a, 's) c

end


module FiniteNbvpaMetaTrapo : sig

	type ('q, 'a) i = ('q, ('a NMVPA.nested)) SimpleMetaTrapo.t
	type ('q, 'a, 's) r = ('q, ('a NMVPA.nested), 's option, unit) MetaTrapo.t
	type ('q, 'a, 's) c = ('q, ('a NMVPA.nested), unit, 's) MetaTrapo.t
	

	val make_i: ('q, 'a, _) NBVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> ('q, 'a) i 
	val make_r: ('q, 'a, 's) NBVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> ('q, 'a, 's) r
	val make_c: ('q, 'a, 's) NBVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> ('q, 'a, 's) c
	
	val make: ('q, 'a, 's) NBVPA.t -> 'q Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> ('q, 'a) i * ('q, 'a, 's) r * ('q, 'a, 's) c

end
