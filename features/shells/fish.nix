{
  self,
  inputs,
  lib',
  ...
}: {
  flake.nixosModules.fish = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
    inherit (pkgs.lib) getExe;
  in {
    programs.fish = {
      enable = true;
      package = self.packages.${system}.fish;
    };
    environment.shells = [self.packages.${system}.fish];
    environment.systemPackages = [pkgs.grc];

    programs.bash.interactiveShellInit = ''
      if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
      then
        shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
        exec ${getExe self.packages.${system}.fish} $LOGIN_OPTION
      fi
    '';
  };

  perSystem = {
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mapAttrs;
    inherit (lib'.shell) mkZoxideInit mkStarshipInit aliases abbrs;
  in {
    packages.fish = inputs.wrapper-modules.wrappers.fish.wrap {
      inherit pkgs;

      shellAliases = aliases pkgs;

      abbreviations = mapAttrs (_: expansion: {inherit expansion;}) abbrs;

      configFile.content = ''
        set fish_greeting

        source ${mkStarshipInit pkgs "fish"}
        source ${mkZoxideInit pkgs "fish"}

        function magic-enter-cmd
          set --local my_magic_command 'ls'
          if command git rev-parse --is-inside-work-tree &>/dev/null
            set my_magic_command "ls && git status"
          end
          echo $my_magic_command
        end

        function haskellEnv
          nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ $argv ])"
        end
      '';

      plugins = with pkgs.fishPlugins; [
        {src = grc.src;}
        {src = fifc.src;}
        {src = fzf.src;}
        {src = plugin-git.src;}
        {src = autopair.src;}
        {src = humantime-fish.src;}
        {src = puffer.src;}
        {src = done.src;}
        {
          src = pkgs.fetchFromGitHub {
            owner = "mattmc3";
            repo = "magic-enter.fish";
            rev = "ddcf5c2cf9ff90c15a724bcdd794a486098492e0";
            hash = "sha256-zDrc2d2VTeTiukRLeezlbj06ICr0AJId/iJx11xPKyo=";
          };
        }
      ];
    };
  };
}
