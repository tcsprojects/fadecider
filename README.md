FADecider
=========

A package for deciding universality and subsumption of omega automata using Ramsey-based methods.

Version 0.6, Copyright (c) 2011-2017

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.de)
- (c) Felix Klaedtke, ETH Zurich (http://www.inf.ethz.ch/personal/felixkl)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Installation

Install the OCaml Package Manager [OPAM](https://opam.ocaml.org).

Then:
```bash	
opam update
opam upgrade
opam switch 4.03.0
eval `opam config env`
opam install ocamlbuild ocamlfind TCSLib extlib
git clone https://github.com/tcsprojects/fadecider.git
cd fadecider
ocaml setup.ml -configure
ocaml setup.ml -build
```


## Benchmarks & Credits
We thank Evan Driscoll, Amanda Burton, and Thomas Reps for providing us with their benchmarks taken from their paper ```Checking Conformance of a Producer and a Consumer```.
		
You can try them out as follows:
	
> ```cat benchmarks/opennwa/assembled-producer-throttle-prod-std.nbvpa | bin/fadecider -s finite_nbvpa -a benchmarks/opennwa/enriched-throttle-cons-std.nbvpa```

> ```cat benchmarks/opennwa/assembled-producer-gzip-prod.nbvpa | bin/fadecider -s finite_nbvpa -a benchmarks/opennwa/enriched-gzip-cons-mod.nbvpa```
		
