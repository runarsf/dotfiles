{ config, pkgs, inputs, outputs, name, ... }:

# Useful tools
#   https://binary.ninja/
#   https://en.wikipedia.org/wiki/List_of_file_signatures
#   https://gchq.github.io/CyberChef/
#   https://www.cachesleuth.com/
#   https://futureboy.us/stegano/decinput.html
#   https://www.dcode.fr/tools-list
#   https://pequalsnp-team.github.io/cheatsheet/steganography-101
#   https://webhook.site/

# {
# TODO https://github.com/redcode-labs/RedNix/issues/10
# $ nixos-container start rednix
# $ systemctl status container@rednix
# $ nixos-container root-login rednix
# imports = [ inputs.rednix.container ];
# } //
outputs.lib.mkModule config "ctf" {
  # containers.rednix.autoStart = false;

  nixos = {
    programs.wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };
    users.users."${name}".extraGroups = [ "wireshark" ];
  };

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

        # inputs.binsider.packages.${pkgs.system}.default
      ] ++ outputs.lib.optionals config.modules.android.enable
      (with pkgs; [ frida-tools unstable.jadx apktool ]);

    file.".local/bin/nc-respond".source = let
      script = pkgs.writeShellApplication {
        name = "nc-respond";
        runtimeInputs = with pkgs; [ coreutils ];
        text = builtins.readFile ./nc-respond.sh;
      };
    in "${script}/bin/nc-respond";
  };
}
