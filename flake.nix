{
  description = "TBD";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-filter
    , pre-commit-hooks
    , xmonad-contrib
    , ...
    }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default self.overlays.xmonad-contrib ];
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
              xmonad-damianfral = self.callCabal2nix "xmonad-damianfral" filteredSrc { };
            });
        });
      };
      overlays.xmonad-contrib = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions
            (old.overrides or (_: _: { }))
            (xmonad-contrib.hoverlay final prev);
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
        };
      };
    in
    rec {
      packages.xmonad-damianfral = pkgs.haskellPackages.xmonad-damianfral;
      packages.default = packages.xmonad-damianfral;

      apps.xmonad-damianfral = flake-utils.lib.mkApp {
        drv = pkgs.haskell.lib.justStaticExecutables (
          packages.xmonad-damianfral.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
          })
        );
      };
      apps.default = apps.xmonad-damianfral;

      devShells.default = pkgs.haskellPackages.shellFor {
        packages = p: [ packages.xmonad-damianfral ];
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

