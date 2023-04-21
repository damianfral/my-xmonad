{
  description = "TBD";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-filter
    , pre-commit-hooks
    , ...
    }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };
      filteredSrc =
        nix-filter.lib {
          root = ./.;
          include = [
            "src/"
            "test/"
            "package.yaml"
            "LICENSE"
          ];
        };
    in
    {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions
            (old.overrides or (_: _: { }))
            (self: super: {
              my-xmonad = self.callCabal2nix "my-xmonad" filteredSrc { };
            });
        });
      };
    }
    //
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = pkgsFor system;
      precommitCheck = pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          actionlint.enable = true;
          hlint.enable = true;
          hpack.enable = true;
          markdownlint.enable = true;
          nil.enable = true;
          nixpkgs-fmt.enable = true;
          ormolu.enable = true;
          statix.enable = true;
        };
      };
    in
    rec {
      packages.my-xmonad = pkgs.haskellPackages.my-xmonad;
      packages.my-xmonad-src = pkgs.stdenv.mkDerivation {
        name = "my-xmonad-src";
        src = ./.;
        installPhase = ''
          mkdir -p $out
          cp src/Main.hs $out/
        '';
      };
      packages.default = packages.my-xmonad;

      apps.my-xmonad = flake-utils.lib.mkApp {
        drv = pkgs.haskell.lib.justStaticExecutables (
          packages.my-xmonad.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
          })
        );
      };
      apps.default = apps.my-xmonad;

      devShells.default = pkgs.haskellPackages.shellFor {
        packages = p: [ packages.my-xmonad ];
        buildInputs = with pkgs; with pkgs.haskellPackages; [
          actionlint
          cabal-install
          ghcid
          haskell-language-server
          hlint
          nil
          nixpkgs-fmt
          ormolu
          statix
        ];
        inherit (precommitCheck) shellHook;
      };

      checks = { pre-commit-check = precommitCheck; };
    });

  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}

