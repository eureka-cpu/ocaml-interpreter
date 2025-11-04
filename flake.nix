{
  description = ''
    An interpreter written in OCaml for the sake of learning OCaml.
  '';

  inputs = {
    opam-nix.url = "github:tweag/opam-nix";

    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.follows = "opam-nix/nixpkgs";
  };

  outputs =
    { self
    , flake-utils
    , opam-nix
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      opamLib = opam-nix.lib.${system};
      ocaml-interpreter = (opamLib.buildOpamProject'
        {
          resolveArgs.with-test = true;
        } ./.
        {
          ocaml-base-compiler = "*";
        }).ocaml-interpreter;
    in
    {
      packages.default = ocaml-interpreter;

      devShells.default = pkgs.mkShell {
        inputsFrom = [ self.packages.${system}.default ];
        buildInputs = with pkgs; [
          nil
          ocamlformat
        ] ++ (with ocamlPackages; [
          ocaml-lsp
          utop
          odoc
        ]);
      };

      formatter = pkgs.nixpkgs-fmt;
    });
}
