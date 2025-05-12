(lang dune 3.16)

(name busqer_bin)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name busqer_bin)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune
  obus
  utop
  lablgtk
  lwt_glib
  cohttp-lwt-unix
  lwt_ssl
  imagelib
  ounit2
  qcheck-ounit)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
