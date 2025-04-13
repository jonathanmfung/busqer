(lang dune 3.16)

(name spotify_dbus)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name spotify_dbus)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune
  obus
  utop
  bogue
  lablgtk
  lwt_glib)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
