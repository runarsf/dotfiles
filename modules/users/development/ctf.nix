{ config, pkgs, inputs, outputs, ... }:

# {
  # TODO https://github.com/redcode-labs/RedNix/issues/10
  # $ nixos-container start rednix
  # $ systemctl status container@rednix
  # $ nixos-container root-login rednix
  # imports = [ inputs.rednix.container ];
# } //
outputs.lib.mkModule config "ctf" {
  # containers.rednix.autoStart = false;

  home = {
    packages = with pkgs; [
      radare2
      ghidra
      sshpass
      file
      fcrackzip
      socat
      unstable.steghide
      wireshark

      binutils
      gdb
      capstone
      checksec
      jq
      yq
      gobuster
      one_gadget
      nmap
      p7zip # HTB archives can't be unpacked by `unzip`...
      patchelf
      wget
    ];

    file.".local/bin/nc-respond".source = let
      script = pkgs.writeShellApplication {
        name = "nc-respond";
        runtimeInputs = with pkgs; [ coreutils ];
        text = builtins.readFile ./nc-respond.sh;
      };
    in "${script}/bin/nc-respond";
  };
}
