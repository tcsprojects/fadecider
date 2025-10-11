(lang dune 3.18)
(name fadecider)
(version 0.7)

(generate_opam_files true)

(source (github tcsprojects/fadecider))

(package
 (name fadecider)
 (maintenance_intent "(latest)")
 (synopsis "A package for deciding universality and subsumption of omega automata using Ramsey-based methods")
 (description "A package for deciding universality and subsumption of omega automata using Ramsey-based methods.")
 (depends
   (ocaml (>= 4.8))
   dune
   (tcs-lib (>= 0.6))
 )
 (license "BSD-3-clause")
 (authors "Oliver Friedmann" "Felix Klaedtke" "Martin Lange")
 (maintainers "Oliver Friedmann" "Felix Klaedtke" "Martin Lange")
 (homepage "https://github.com/tcsprojects/fadecider")
 (bug_reports "https://github.com/tcsprojects/fadecider/issues")
)
