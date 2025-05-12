let
  pkgs =
    import
      # f9f0d5 is 241203
      (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f9f0d5c5380be0a599b1fb54641fa99af8281539.tar.gz")
      { };
  obus = pkgs.ocamlPackages.buildDunePackage {
    pname = "obus";
    version = "1.2.5";

    src = pkgs.fetchFromGitHub {
      owner = "ocaml-community";
      repo = "obus";
      rev = "6d9470dbc22b607ea4eadc734ea9b547d8bf5c51";
      hash = "sha256-0nq41c35IICCQ8dJhjuWoIyaBms+/KOwIqumqtCXP7g=";
    };

    nativeBuildInputs = [
      pkgs.ocamlPackages.menhir
    ];
    buildInputs = with pkgs.ocamlPackages; [
      lwt_ppx
      lwt_log
      xmlm
      lwt_react
      menhirLib
    ];
  };
  lwt_glib = pkgs.ocamlPackages.buildDunePackage {
    pname = "lwt_glib";
    version = "1.1.1";

    src = pkgs.fetchFromGitHub {
      owner = "ocsigen";
      repo = "lwt_glib";
      rev = "72e67ca04cac0219bdb891b17d27d089d02551e1";
      hash = "sha256-qZ3vqqwvDL6Xe65Wqqj2/qg+Ia44Sr26SF6siMtf0fE=";
    };

    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [
      pkgs.ocamlPackages.lwt
      pkgs.glib
    ];
    doCheck = true;
  };
in
pkgs.mkShell {
  pname = "busqer";
  # src = ./.;
  buildInputs = with pkgs; [
    pkg-config

    # GTK
    glib
    gtk2

    # TLS for Conduit/Cohttp
    ocamlPackages.lwt_ssl

    # OUnit
    ocamlPackages.ounit
    ocamlPackages.qcheck-ounit
  ];
  packages = with pkgs; [
    pkg-config
    ocamlPackages.ocaml
    ocamlPackages.dune_3

    # Direct Deps
    ocamlPackages.lablgtk3
    ocamlPackages.cohttp-lwt-unix
    ocamlPackages.lacaml
    ocamlPackages.imagelib
    obus
    lwt_glib

    # Indirect Deps
    ocamlPackages.lwt_ssl # TLS for Conduit/Cohttp
    ocamlPackages.ounit
    ocamlPackages.xmlm
    imagemagick # convert for imagelib jpg -> png

    # for scripts/
    graphviz

    # dev
    ocamlPackages.utop
    ocamlPackages.ocaml-lsp
    ocamlformat
  ];
}
