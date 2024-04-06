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
          include = [ "src/" "test/" "package.yaml" "LICENSE" ];
        };

    in
    {
      overlays.default = final: prev: {
        xmonad-damianfral = prev.haskell.lib.justStaticExecutables (
          final.haskellPackages.xmonad-damianfral.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
          })
        );
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

      nixosModules.xmonad-damianfral = { pkgs, lib, config, ... }: with lib;
        let cfg = config.services.xserver.windowManager.xmonad-damianfral; in {
          options = {
            services.xserver.windowManager.xmonad-damianfral = {
              enable = mkEnableOption "xmonad-damianfral";
              xmobarConfig = mkOption {
                type = types.path;
                default = ./xmobarrc;
              };
              wallpaper = mkOption {
                type = types.str;
                default = "${pkgs.nixos-artwork.wallpapers.dracula}/share/backgrounds/nixos/nix-wallpaper-dracula.png";
              };
            };
          };
          config = mkIf cfg.enable {
            services.xserver.enable = true;
            services.xserver.displayManager.defaultSession = "xmonad-damianfral";
            services.xserver.displayManager.session = [{
              manage = "desktop";
              name = "xmonad-damianfral";
              start = ''
                systemd-cat -t xmonad-damianfral -- \
                  ${lib.getExe self.packages.${pkgs.system}.xmonad-damianfral} \
                   --xmobar-config ${cfg.xmobarConfig} \
                   --wallpaper ${cfg.wallpaper} &
                waitPID=$!
              '';
            }];
            environment.systemPackages = with pkgs;
              [ xmobar xwallpaper kitty maim pulsemixer playerctl ];
          };

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
        drv = packages.xmonad-damianfral;
      };
      apps.xmonad-damianfral-vm-interactive = flake-utils.lib.mkApp {
        drv = checks.xmonad-damianfral-vm.driverInteractive;
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

      checks = rec {
        pre-commit-check = precommitCheck;
        xmonad-damianfral-screenshot = pkgs.stdenv.mkDerivation {
          name = "xmonad-damianfral-screenshot";
          phases = [ "checkPhase" "installPhase" ];
          src = ./golden-screenshots;
          buildInputs = [ pkgs.imagemagick xmonad-damianfral-vm ];
          checkPhase = ''
            METRIC=$(magick compare -metric AE ${xmonad-damianfral-vm}/screenshot.000.png $src/screenshot.000.png )
            THRESHOLD=(( 16 * 1080 / 4))
            ((METRIC < THRESHOLD)) && true
          '';
          installPhase = ''
            mkdir -p $out
            cp $src/screenshot.000.png $out/screenshot.a.png
            cp ${xmonad-damianfral-vm}/screenshot.000.png $out/screnshot.b.png
          '';

        };
        xmonad-damianfral-vm = let name = "test_node"; in nixpkgs.lib.nixos.runTest {
          name = "nixos-test-xmonad-damianfral";
          hostPkgs = pkgs;
          enableOCR = false;
          nodes."${name}" = {
            imports = [ self.nixosModules.xmonad-damianfral ];
            boot.loader.systemd-boot.enable = true;
            boot.loader.efi.canTouchEfiVariables = true;
            services.xserver.displayManager.lightdm.enable = true;
            services.xserver.displayManager.autoLogin.enable = true;
            services.xserver.displayManager.autoLogin.user = "test";
            users.users.test = {
              description = "test";
              initialPassword = "0000";
              isNormalUser = true;
              extraGroups = [ "wheel" "sudo" ];
            };
            virtualisation.graphics = true;
            virtualisation.cores = 2;
            virtualisation.resolution = { x = 1920; y = 1080; };

            services.xserver.windowManager.xmonad-damianfral.enable = true;
          };
          testScript = ''
            start_all()

            with subtest("it launches basic services"):
              ${name}.wait_for_unit("default.target")
              ${name}.wait_for_unit("network.target")
              ${name}.wait_for_unit("graphical.target")
              ${name}.wait_for_unit("display-manager.service")
              ${name}.wait_for_unit("multi-user.target")

            with subtest("it logs in and starts xmonad"):
              ${name}.wait_for_x()
              ${name}.succeed("pgrep xmonad")

            with subtest("it looks as expected"):
              # Just take a screenshot, another derivation will check it.
              ${name}.screenshot("screenshot.000.png")
          '';
        };
      };
    });
  nixConfig = {
    extra-substituters = [
      "https://opensource.cachix.org"
      "https://cache.garnix.io"
    ];

    extra-trusted-public-keys = [
      "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}

