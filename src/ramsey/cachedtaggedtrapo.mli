open Tcsbasedata;;
open Tcsset;;
open Tcsautomata;;
open Trapo;;
open Cachedtrapo;;
open Taggedtrapo;;



module CachedTaggedTrapo : sig

	type ('p, 'r, 's, 'a) s
	
	val make: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> 's -> ('p, 'r, 's, 'a) s

	val format: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('p, 'r, 's, 'a) s -> int -> string

	val format_word: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('p, 'r, 's, 'a) s -> int -> string
	
	val decoration: ('p, 'r, 's, 'a) s -> int -> 'p * 'r * int * 'p * 's

	val get_word: ('p, 'r, 's, 'a) s -> int -> 'a Alphabet.word

	val single: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('p, 'r, 's, 'a) s -> 'a -> 'p -> 'r -> 'p -> 's -> int

	val is_idempotent: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'x) CachedTrapo.s -> ('p, 'r, 's, 'a) s -> int -> bool

	val image: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('p, 'r, 's, 'a) s -> int -> ('q * 'x) Iterators.iterator -> ('q * 'y) TreeSet.t
	
	val preimage: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('p, 'r, 's, 'a) s -> int -> ('q * 'y) TreeSet.t -> ('q * 'x) TreeSet.t

	val compose_by_composer: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> 
							 ('p, 'r, 's, 'a) s -> ('p, 'ss, 't, 'a) s -> ('p, 'r, 't, 'a) s ->
							 (int -> int -> int) -> int -> int -> int

	val compose: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'yy, 'z, 'p, 'ss, 't) MetaTaggedTrapo.t -> 
		           ('q, 'a, 'x, 'y) CachedTrapo.s -> ('q, 'a, 'yy, 'z) CachedTrapo.s -> ('q, 'a, 'x, 'z) CachedTrapo.s ->
							 ('p, 'r, 's, 'a) s -> ('p, 'ss, 't, 'a) s -> ('p, 'r, 't, 'a) s ->
							 ('y -> 'yy) -> int -> int -> int

	val compose_right_by_composer: ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t ->
										 ('p, 's, 't, 'a) s -> ('p, unit, unit, 'a) s ->
										 (int -> int -> int) ->
										 int -> int -> int

	val compose_right: ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t -> ('q, 'a, unit, unit, 'p, unit, unit) MetaTaggedTrapo.t ->
									   ('q, 'a, 'y, 'z) CachedTrapo.s -> ('q, 'a, unit, unit) CachedTrapo.s ->
										 ('p, 's, 't, 'a) s -> ('p, unit, unit, 'a) s ->
										 int -> int -> int

	val collapse_right: ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t -> ('q, 'a, 'y, unit, 'p, 's, unit) MetaTaggedTrapo.t ->
											('q, 'a, 'y, 'z) CachedTrapo.s -> ('p, 's, 't, 'a) s -> ('q, 'a, 'y, unit) CachedTrapo.s -> ('p, 's, unit, 'a) s ->
		                  int -> int
											
	val project_left: ('q, 'a, 'y, 'z, 'p, 's, 't) MetaTaggedTrapo.t ->
										('q, 'a, 'y, 'z) CachedTrapo.s -> ('p, 's, 't, 'a) s -> ('q, 'a, unit, 'z) CachedTrapo.s -> ('p, unit, 't, 'a) s ->
										int -> 'y -> 's -> int option

end



module CachedTaggedTrapoSet : sig

	type ('p, 'r) t

	val empty: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('p, 'r) t
	
	val is_empty: ('p, 'r) t -> bool

	val cardinal: ('p, 'r) t -> int
	
	val add: ('p, 'r, 's, 'a) CachedTaggedTrapo.s -> int -> ('p, 'r) t -> ('p, 'r) t

	val iter: (int -> unit) -> ('p, 'r) t -> unit

	val iter_sub: (int -> unit) -> 'p * 'r -> ('p, 'r) t -> unit
	
	val fold_sub: (int -> 'b -> 'b) -> 'p * 'r -> ('p, 'r) t -> 'b -> 'b

	val union: ('p, 'r) t -> ('p, 'r) t -> ('p, 'r) t

	val diff: ('p, 'r) t -> ('p, 'r) t -> ('p, 'r) t

	val fold: (int -> 'b -> 'b) -> ('p, 'r) t -> 'b -> 'b

	val singles: ('q, 'a, 'x, 'y, 'p, 'r, 's) MetaTaggedTrapo.t -> ('q, 'a, 'x, 'y) CachedTrapo.s -> ('p, 'r, 's, 'a) CachedTaggedTrapo.s -> ('p, 'r) t
	
	val join: ('qqq, 'aaa, 'xxx, 'yyy, 'ppp, 'rrr, 'sss) MetaTaggedTrapo.t ->
		        ('p, 'r, 's, 'a) CachedTaggedTrapo.s -> ('pp, 'rr, 'ss, 'a) CachedTaggedTrapo.s -> ('ppp, 'rrr, 'sss, 'a) CachedTaggedTrapo.s ->
						('p * 's -> ('pp * 'rr) -> bool) -> (int -> int -> int) -> ('p, 'r) t -> ('pp, 'rr) t -> ('ppp, 'rrr) t

end



module SimpleCachedTaggedTrapo : sig

	type ('p, 'a) s = ('p, unit, unit, 'a) CachedTaggedTrapo.s

	val make: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('p, 'a) s

	val format_word: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a) SimpleCachedTrapo.s -> ('p, 'a) s -> int -> string

	val format: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a) SimpleCachedTrapo.s -> ('p, 'a) s -> int -> string
	
	val get_word: ('p, 'a) s -> int -> 'a Alphabet.word

	val decoration: ('p, 'a) s -> int -> 'p * int * 'p
	
	val compose_by_composer: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('p, 'a) s -> (int -> int -> int) -> int -> int -> int

	val compose: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('p, 'a) s -> ('q, 'a) SimpleCachedTrapo.s -> int -> int -> int
	
  val is_idempotent: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('q, 'a) SimpleCachedTrapo.s -> ('p, 'a) s -> int -> bool	
	
	val image: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('p, 'a) s -> ('q, 'a) SimpleCachedTrapo.s -> int -> 'q Iterators.iterator -> 'q TreeSet.t
	
	val preimage: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('p, 'a) s -> ('q, 'a) SimpleCachedTrapo.s -> int -> 'q TreeSet.t -> 'q TreeSet.t

  val good_states: ('q, 'a, 'p) SimpleMetaTaggedTrapo.t -> ('p, 'a) s -> ('q, 'a) SimpleCachedTrapo.s -> int -> 'q TreeSet.t

end	

