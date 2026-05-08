{
  description = "TBD";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    xmonad-contrib,
    ...
  } @ inputs: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default] ++ inputs.xmonad-contrib.overlays;
      };
    filteredSrc = nix-filter.lib {
      root = ./.;
      include = ["src/" "test/" "package.yaml" "LICENSE"];
    };
  in
    {
      overlays.default = final: prev: let
        xmobar =
          final.haskell.lib.justStaticExecutables
          (prev.haskellPackages.xmobar.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ ["--ghc-options=-O2"];
          }));
        tools = with final; [
          xmobar
          xwallpaper
          maim
          pulsemixer
          playerctl
          xclip
          haskellPackages.greenclip
        ];
      in {
        xmonad-damianfral = prev.haskell.lib.justStaticExecutables (
          final.haskellPackages.xmonad-damianfral.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ ["--ghc-options=-O2"];
          })
        );
        haskellPackages = prev.haskellPackages.override (old: {
          overrides =
            final.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (self: super: {
              xmonad-damianfral = (self.callCabal2nix "xmonad-damianfral" filteredSrc {}).overrideAttrs (oldAttrs: {
                nativeBuildInputs =
                  oldAttrs.nativeBuildInputs ++ [final.makeWrapper];
                postInstall =
                  (oldAttrs.postInstall or "")
                  + ''
                    wrapProgram $out/bin/xmonad-damianfral \
                      --suffix PATH : ${final.lib.makeBinPath tools}
                  '';
              });
            });
        });
      };

      nixosModules.xmonad-damianfral = {
        pkgs,
        lib,
        config,
        ...
      }:
        with lib; let
          cfg = config.services.xserver.windowManager.xmonad-damianfral;
          mkIfEnable = mkIf cfg.enable;
        in {
          options = {
            services.xserver.windowManager.xmonad-damianfral = {
              enable = mkEnableOption "xmonad-damianfral";
              xmobarConfig = mkOption {
                type = types.path;
                default = ./xmobarrc;
              };
              wallpaper = mkOption {
                type = types.path;
                default =
                  pkgs.nixos-artwork.wallpapers.dracula
                  + "/share/backgrounds/nixos/nix-wallpaper-dracula.png";
              };
              terminal = mkPackageOption pkgs "kitty" {};
              screenshotDir = mkOption {
                type = types.str;
                default = "~/screenshots";
              };
            };
          };

          config = mkIfEnable {
            nixpkgs.overlays = [self.overlays.default];
            services.xserver.enable = true;
            services.xserver.displayManager = {
              session = [
                {
                  manage = "desktop";
                  name = "xmonad-damianfral";
                  start = ''
                    systemd-cat -t xmonad-damianfral -- \
                      xmonad-damianfral \
                       --xmobar-config ${cfg.xmobarConfig} \
                       --wallpaper ${cfg.wallpaper} \
                       --term ${getExe cfg.terminal} \
                       --screenshot-dir ${cfg.screenshotDir} &
                    waitPID=$!
                  '';
                }
              ];
            };
            environment.systemPackages = [pkgs.xmonad-damianfral cfg.terminal];

            fonts = {
              enableDefaultPackages = true;
              packages = [pkgs.nerd-fonts.anonymice];
              fontconfig = {
                enable = true;
                defaultFonts.monospace = ["AnonymicePro Nerd Font"];
              };
            };
          };
        };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = pkgsFor system;
      precommitCheck = pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          actionlint.enable = true;
          hlint.enable = true;
          hpack.enable = true;
          markdownlint.enable = true;
          nil.enable = true;
          alejandra.enable = true;
          ormolu.enable = true;
        };
      };
    in rec {
      packages.default = pkgs.xmonad-damianfral;

      packages.xmonad-damianfral-vm = inputs.nixpkgs.lib.nixos.runTest rec {
        name = "nixos_test_xmonad_damianfral";
        hostPkgs = import inputs.nixpkgs {system = "x86_64-linux";};
        enableOCR = false;
        nodes."${name}" = {
          imports = [self.nixosModules.xmonad-damianfral];
          boot.loader.systemd-boot.enable = true;
          boot.loader.efi.canTouchEfiVariables = true;
          services.xserver.displayManager.lightdm.enable = true;
          services.displayManager.autoLogin.enable = true;
          services.displayManager.autoLogin.user = "test";
          users.users.test = {
            description = "test";
            initialPassword = "0000";
            isNormalUser = true;
            extraGroups = ["wheel" "sudo"];
          };
          virtualisation.graphics = true;
          virtualisation.cores = 2;
          virtualisation.resolution = {
            x = 1920;
            y = 1080;
          };
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

      packages.xmonad-damianfral-screenshot = pkgs.stdenv.mkDerivation {
        name = "xmonad-damianfral-screenshot";
        phases = ["buildPhase" "installPhase"];
        src = ./golden-screenshots;
        buildInputs = [pkgs.imagemagick];
        buildPhase = with packages; ''
          set -xue

          RESULT=$(compare -metric AE ${xmonad-damianfral-vm}/screenshot.000.png $src/screenshot.000.png screenshot.diff.png 2>&1) || true
          METRIC=$(echo $RESULT | cut -f 1 -d ' ')
          echo $METRIC

          THRESHOLD=1000

          if (( $(echo "$METRIC $THRESHOLD" | awk '{print ($1 < $2)}') )); then
            echo "Screenshot similarity test PASSED: $METRIC < $THRESHOLD"
          else
            echo "Screenshot similarity test FAILED: $METRIC >= $THRESHOLD"
            exit 1
          fi
        '';
        installPhase = with packages; ''
          mkdir -p $out
          cp $src/screenshot.000.png $out/screenshot.a.png
          cp ${xmonad-damianfral-vm}/screenshot.000.png $out/screenshot.b.png
          cp screenshot.diff.png $out/
        '';
      };

      apps.xmonad-damianfral-vm-interactive = flake-utils.lib.mkApp {
        drv = checks.xmonad-damianfral-vm.driverInteractive;
        name = "nixos-test-driver";
      };

      devShells.default = pkgs.haskellPackages.shellFor {
        packages = p: [p.xmonad-damianfral];
        buildInputs = with pkgs;
        with pkgs.haskellPackages; [
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

      checks = {pre-commit-check = precommitCheck;};
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
