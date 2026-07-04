{self, ...}: {
  flake.nixosModules.ctf = {
    config,
    lib,
    pkgs,
    ...
  }: {
    programs.wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };
    users.groups.wireshark.members = config.primaryUsers;
  };

  flake.homeModules.ctf = {
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) optionals;
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    home = {
      packages = with pkgs;
        [
          radare2
          ghidra
          sshpass
          file
          fcrackzip
          socat
          unstable.steghide
          metasploit
          # pwntools # this collides with moreutils, which we need for sponge
          exiftool
          binwalk

          binutils
          foremost
          gdb
          capstone
          jq
          yq
          gobuster
          one_gadget
          nmap
          p7zip # HTB archives can't be unpacked by `unzip`...
          patchelf
          wget
          burpsuite

          (self.packages.${system}.nc-respond)
          # inputs.binsider.packages.${system}.default
        ]
        ++ optionals nixpkgs.config.android_sdk.accept_license [
          frida-tools
          unstable.jadx
          apktool
        ];
    };
  };
}
