{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [ (import rust-overlay) ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };

      rust-bin = pkgs.rust-bin.stable.latest.default.override {
        extensions = [
          "rust-analyzer"
          "rust-src"
        ];
      };

      buildInputs = [
        rust-bin
      ];
      nativeBuildInputs = with pkgs; [
        # For driver script
        bash
        gcc
        gdb
        jq
        # For book tests
        python3
      ];
    in
    {
      devShells.default = pkgs.mkShell {
        inherit buildInputs nativeBuildInputs;
      };
    });
}
