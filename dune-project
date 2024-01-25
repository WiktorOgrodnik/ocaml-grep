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
 (synopsis "grep")
 (description "Search your regex patterns")
 (depends ocaml dune core ppx_jane ANSITerminal))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
