FADecider
=========

A package for deciding universality and subsumption of omega automata using Ramsey-based methods.

Version 0.7, Copyright (c) 2011-2015, BSD 3 LICENSE

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.de)
- (c) Felix Klaedtke, ETH Zurich (http://www.inf.ethz.ch/personal/felixkl)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## OPAM

You can install this package via OPAM under the name `fadecider`.


## Commands


### Build

```
dune build
```

### Release

1. Change version in `dune-project`.
2. Update `CHANGES.md`.
3. Run `dune build`.
4. Commit
```
  git status
  git add -A
  git commit -m "message"
  git tag v0.x [--force]
  git push origin master --tags [--force]
```
5. Release
```
  dune-release tag
  dune-release distrib
  dune-release publish
  dune-release opam pkg
  dune-release opam submit
```  



## Benchmarks & Credits
We thank Evan Driscoll, Amanda Burton, and Thomas Reps for providing us with their benchmarks taken from their paper ```Checking Conformance of a Producer and a Consumer```.


## Test

You can try them out as follows:
	
> ```cat benchmarks/opennwa/assembled-producer-throttle-prod-std.nbvpa | bin/fadecider -s finite_nbvpa -a benchmarks/opennwa/enriched-throttle-cons-std.nbvpa```

> ```cat benchmarks/opennwa/assembled-producer-gzip-prod.nbvpa | bin/fadecider -s finite_nbvpa -a benchmarks/opennwa/enriched-gzip-cons-mod.nbvpa```
		
