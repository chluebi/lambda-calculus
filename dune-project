(lang dune 3.4)
(using menhir 2.0)
(name lambda)

; https://dune.readthedocs.io/en/stable/opam.html#generating-opam-files
(generate_opam_files true)

(cram enable)

; https://dune.readthedocs.io/en/stable/concepts.html#package-specification
(package
 (name lambda)
 (synopsis "Test"))