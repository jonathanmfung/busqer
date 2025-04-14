let
  pkgs =
    import
      # f9f0d5 is 241203
      (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f9f0d5c5380be0a599b1fb54641fa99af8281539.tar.gz")
      { };
in
pkgs.mkShell {
  pname = "water_sort";
  src = ./.;
  buildInputs = with pkgs; [
    pkg-config

    # GTK
    glib
    gtk2
  ];
  packages = with pkgs; [
    opam
    ocamlPackages.ocaml
    ocamlPackages.dune_3
    # ocamlPackages.findlib
    ocamlPackages.utop
    # ocamlPackages.odoc
    ocamlPackages.ocaml-lsp
    ocamlformat
    ocamlPackages.lablgtk # Version 2.18
  ];
}
