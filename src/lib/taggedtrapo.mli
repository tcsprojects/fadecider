open Tcsbasedata;;
open Tcsset;;
open Tcsautomata;;
open Trapo;;


module MetaTaggedTrapo : sig

	type ('q, 'a, 'x, 'y, 'p, 'r, 's) t
	
	val make: ('q, 'a, 'x, 'y) MetaTrapo.t -> ('p, 'a, 'r, 's) MetaTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t
	
	val left: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y) MetaTrapo.t
	
	val right: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('p, 'a, 'r, 's) MetaTrapo.t
	
end;;


module TaggedTrapo : sig

	type ('q, 'a, 'x, 'y, 'p, 'r, 's) t 

	val format_transitions: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> string 
	
	val format_word: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> string
	
	val format: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> string

	val get_word: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> 'a Alphabet.word
	
	val get_trapo: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y) Trapo.t
	
	val make: ('q, 'a, 'x, 'y) Trapo.t -> ('p * 'r * 'p * 's * int) -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t 

	val compare_words: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> int
	
	val compare_tagged_transitions: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('p * 'r * 'p * 's * int) -> ('p * 'r * 'p * 's * int) -> int

	val compare_transitions: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> int
	
	val compare_full: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> int

	val compare: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> int

	val single: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> 'a -> 'p -> 'r -> 'p -> 's -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t

	val decoration: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> 'p * 'r * int * 'p * 's

	val is_idempotent: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'x, 'p, 'r, 's) t -> bool

	val image: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q * 'x) Iterators.iterator -> ('q * 'y) TreeSet.t

	val preimage: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q * 'y) TreeSet.t -> ('q * 'x) TreeSet.t

	val compose: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'yy, 'z, 'p, 'ss, 't) MetaTaggedTrapo.t -> ('y -> 'yy) ->
	             ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'yy, 'z, 'p, 'ss, 't) t -> ('q, 'a, 'x, 'z, 'p, 'r, 't) t

	val compose_left: ('q, 'a, unit, unit, 'p, unit, unit) MetaTaggedTrapo.t -> ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t ->
	                  ('q, 'a, unit, unit, 'p, unit, unit) t -> ('q, 'a, 'y, 'z, 'p, 's, 't) t -> ('q, 'a, 'y, 'z, 'p, 's, 't) t

	val compose_right: ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t -> ('q, 'a, unit, unit, 'p, unit, unit) MetaTaggedTrapo.t ->
	                   ('q, 'a, 'y, 'z, 'p, 's, 't) t -> ('q, 'a, unit, unit, 'p, unit, unit) t -> ('q, 'a, 'y, 'z, 'p, 's, 't) t

	val collapse_right: ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t -> ('q, 'a, 'y, unit, 'p, 's, unit) MetaTaggedTrapo.t ->
		                  ('q, 'a, 'y, 'z, 'p, 's, 't) t -> ('q, 'a, 'y, unit, 'p, 's, unit) t
											
	val project_left: ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t -> ('q, 'a, 'y, 'z, 'p, 's, 't) t ->
		                'y -> 's -> ('q, 'a, unit, 'z, 'p, unit, 't) t option

end;;



module TaggedTrapoSet : sig

	type ('q, 'a, 'x, 'y, 'p, 'r, 's) t 
	
	val trapo_proj: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t ->  ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y) TrapoSet.t

	val format: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> string
	
	val empty: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t

	val mem: ('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> bool

	val is_empty: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> bool

	val fold: (('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> 'b -> 'b) -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> 'b -> 'b
	
	val iter: (('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> unit) -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> unit

	val cardinal: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> int

	val add: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t

	val fold_sub: (('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> 'b -> 'b) -> 'p * 'r -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> 'b -> 'b
	
	val iter_sub: (('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> unit) -> 'p * 'r -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> unit

	val union: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t

	val diff: ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t

	val singles: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t
	
	val join: ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) MetaTaggedTrapo.t -> ('p * 's -> ('pp * 'rr) -> bool) -> (('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> ('qq, 'aa, 'xx, 'yy, 'pp, 'rr, 'ss) TaggedTrapo.t -> ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) TaggedTrapo.t) -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('qq, 'aa, 'xx, 'yy, 'pp, 'rr, 'ss) t -> ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) t

	val conditional_join_union: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('qq, 'aa, 'xx, 'yy, 'pp, 'rr, 'ss) MetaTaggedTrapo.t -> ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) MetaTaggedTrapo.t -> ('p * 's -> ('pp * 'rr) -> bool) -> (('q, 'a, 'x, 'y, 'p, 'r, 's) TaggedTrapo.t -> ('qq, 'aa, 'xx, 'yy, 'pp, 'rr, 'ss) TaggedTrapo.t -> ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) TaggedTrapo.t) -> ('q, 'a, 'x, 'y, 'p, 'r, 's) t -> ('qq, 'aa, 'xx, 'yy, 'pp, 'rr, 'ss) t -> ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) t -> (('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) TaggedTrapo.t -> bool) -> (('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) TaggedTrapo.t -> bool) -> ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) t

end;;



module TaggedStatesetSet : sig

	type ('q, 'a, 'p) t
	
	val empty: ('q, 'a, _, _, 'p, _, _) MetaTaggedTrapo.t -> ('q, 'a, 'p) t
	
	val epsilon: ('q, 'a, _, _, 'p, _, _) MetaTaggedTrapo.t -> ('q, 'a, 'p) t
	
	val subsumed: ('q, 'a, 'p) t -> 'p -> 'q TreeSet.t -> bool

	val add: ('q, 'a, _, _, 'p, _, _) MetaTaggedTrapo.t -> ('q, 'a, 'p) t -> 'p -> 'q TreeSet.t -> 'a Alphabet.word -> ('q, 'a, 'p) t
	
	val union: ('q, 'a, 'p) t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t
	
	val reacher_idempot_match: ('q, 'a, _, _, 'p, _, _) MetaTaggedTrapo.t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t -> string option

end


module SimpleMetaTaggedTrapo : sig

	type ('q, 'a, 'p) t = ('q, 'a, unit, unit, 'p, unit, unit) MetaTaggedTrapo.t
	
	val make: 'a Alphabet.alphabet -> 'q Domain.t -> 'q -> ('q -> int) -> (int -> int -> int) -> ('q -> 'a -> 'q Iterators.iterator) -> 'q Iterators.iterator -> 'a Iterators.iterator -> 'p Domain.t -> 'p -> ('p -> int) -> (int -> int -> int) -> ('p -> 'a -> 'p Iterators.iterator) -> 'p Iterators.iterator -> ('q, 'a, 'p) t

end;;


module SimpleTaggedTrapo : sig

	type ('q, 'a, 'p) t = ('q, 'a, unit, unit, 'p, unit, unit) TaggedTrapo.t
	
	val format_transitions: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> string 
	
	val format_word: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> string

	val format: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> string

	val get_word: ('q, 'a, 'p) t -> 'a Alphabet.word

	val compare_words: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t -> int
	
	val compare_transitions: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t -> int
	
	val compare_full: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t -> int

	val compare: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t -> int

	val single: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> 'a -> 'p -> 'p -> ('q, 'a, 'p) t

	val decoration: ('q, 'a, 'p) t -> 'p * int * 'p
	
	val is_idempotent: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> bool

	val image: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> 'q Iterators.iterator -> 'q TreeSet.t

	val preimage: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> 'q TreeSet.t -> 'q TreeSet.t

	val good_states: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> 'q TreeSet.t

	val compose: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t -> ('q, 'a, 'p) t

end	

	

module NpaMetaTaggedTrapo : sig

	type ('q, 'a, 'p) t = ('q, 'a, 'p) SimpleMetaTaggedTrapo.t

	val make: ('q, 'a) NPA.t -> 'q Iterators.iterator -> ('p, 'a) NPA.t -> 'p Iterators.iterator -> 'a Iterators.iterator -> ('q, 'a, 'p) t

end


module FiniteNbaMetaTaggedTrapo : sig

	type ('q, 'a, 'p) t = ('q, 'a, 'p) SimpleMetaTaggedTrapo.t

	val make: ('q, 'a) NBA.t -> 'q Iterators.iterator -> ('p, 'a) NBA.t -> 'p Iterators.iterator -> 'a Iterators.iterator -> ('q, 'a, 'p) t

end



module NpvpaMetaTaggedTrapo : sig

	type ('q, 'a, 'p) i = ('q, ('a NMVPA.nested), 'p) SimpleMetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) r = ('q, ('a NMVPA.nested), 's option, unit, 'p, 'r option, unit) MetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) c = ('q, ('a NMVPA.nested), unit, 's, 'p, unit, 'r) MetaTaggedTrapo.t
	

	val make_i: ('q, 'a, _) NPVPA.t -> 'q Iterators.iterator -> ('p, 'a, _) NPVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> ('q, 'a, 'p) i 
	val make_r: ('q, 'a, 's) NPVPA.t -> 'q Iterators.iterator -> ('p, 'a, 'r) NPVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> 'r Iterators.iterator -> ('q, 'a, 's, 'p, 'r) r
	val make_c: ('q, 'a, 's) NPVPA.t -> 'q Iterators.iterator -> ('p, 'a, 'r) NPVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> 'r Iterators.iterator -> ('q, 'a, 's, 'p, 'r) c
	
	val make: ('q, 'a, 's) NPVPA.t -> 'q Iterators.iterator -> ('p, 'a, 'r) NPVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> 'r Iterators.iterator -> ('q, 'a, 'p) i * ('q, 'a, 's, 'p, 'r) r * ('q, 'a, 's, 'p, 'r) c

end


module FiniteNbvpaMetaTaggedTrapo : sig

	type ('q, 'a, 'p) i = ('q, ('a NMVPA.nested), 'p) SimpleMetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) r = ('q, ('a NMVPA.nested), 's option, unit, 'p, 'r option, unit) MetaTaggedTrapo.t
	type ('q, 'a, 's, 'p, 'r) c = ('q, ('a NMVPA.nested), unit, 's, 'p, unit, 'r) MetaTaggedTrapo.t
	

	val make_i: ('q, 'a, _) NBVPA.t -> 'q Iterators.iterator -> ('p, 'a, _) NBVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> ('q, 'a, 'p) i 
	val make_r: ('q, 'a, 's) NBVPA.t -> 'q Iterators.iterator -> ('p, 'a, 'r) NBVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> 'r Iterators.iterator -> ('q, 'a, 's, 'p, 'r) r
	val make_c: ('q, 'a, 's) NBVPA.t -> 'q Iterators.iterator -> ('p, 'a, 'r) NBVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> 'r Iterators.iterator -> ('q, 'a, 's, 'p, 'r) c
	
	val make: ('q, 'a, 's) NBVPA.t -> 'q Iterators.iterator -> ('p, 'a, 'r) NBVPA.t -> 'p Iterators.iterator -> ('a NMVPA.nested) Iterators.iterator -> 's Iterators.iterator -> 'r Iterators.iterator -> ('q, 'a, 'p) i * ('q, 'a, 's, 'p, 'r) r * ('q, 'a, 's, 'p, 'r) c

end


