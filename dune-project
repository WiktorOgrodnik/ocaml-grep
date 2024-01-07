(lang dune 3.12)

(name ogrep)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Wiktor Ogrodnik")

(maintainers "Wiktor Ogrodnik")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ogrep)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
