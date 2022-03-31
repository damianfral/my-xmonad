{
  description = "my xmonad config";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/master"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    xmonad = { url = github:xmonad/xmonad; };
    xmonad-contrib = { url = github:xmonad/xmonad-contrib; };
    xmonad-extras = { url = github:xmonad/xmonad-extras; flake = false; };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    } @ inputs:
    let
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = inputs.xmonad-contrib.overlays;
        };
    in
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs = pkgsFor system;
          mk-my-xmonad = returnShellEnv:
            pkgs.haskellPackages.developPackage {
              root = ./.;
              source-overrides = { };
              overrides = new: old:
                let
                  dontCheck = pkgs.haskell.lib.dontCheck;
                  unmarkBroken = pkgs.haskell.lib.unmarkBroken;
                in
                { };
              modifier = drv:
                if returnShellEnv
                then
                  pkgs.haskell.lib.addBuildTools drv
                    (
                      with pkgs.haskellPackages; [
                        cabal-install
                        ghcid
                        hpack
                        haskell-language-server
                        hlint
                        ormolu
                      ]
                    )
                else pkgs.haskell.lib.justStaticExecutables drv;
              inherit returnShellEnv;
            };
        in
        rec {
          packages = rec {
            my-xmonad = mk-my-xmonad false;
            my-xmonad-shell = mk-my-xmonad true;
            my-xmonad-src = pkgs.stdenv.mkDerivation
              {
                name = "my-xmonad-src";
                src = ./.;
                installPhase = ''
                  mkdir -p $out
                  cp app/Main.hs $out/
                '';
              };
          };
          defaultPackage = packages.my-xmonad;

          devShell = packages.my-xmonad-shell;

          overlay = final: prev:
            packages // {
              haskellPackages = prev.haskellPackages.extend
                (
                  hself: hsuper:
                    {
                      monad-extras = hself.callCabal2nix "xmonad-extras" inputs.xmonad-extras { };
                    }
                );
            };
          overlays = [ overlay ] ++ inputs.xmonad-contrib.overlays;
        }
      );
}
