{
  pkgs,
  name,
  outputs,
  config,
  ...
}:

outputs.lib.mkModule config "android" {
  nixos = {
    programs.adb.enable = true;
    users.users."${name}".extraGroups = [
      "adbusers"
      "plugdev"
      "kvm"
    ];
    services.udev.packages = with pkgs; [ android-udev-rules ];
    environment.systemPackages = with pkgs; [ android-tools ];
  };

  home.packages = with pkgs.unstable; [
    flutter
    graphite2
    gtk3
  ];

  nixpkgs.config.android_sdk.accept_license = true;

  home.file.".local/bin/adbrute".source =
    let
      script = pkgs.writers.writeBashBin "adbrute" ''
        set -uo pipefail

        IP="''${1:?Usage: ''${0} <IP>}"
        PORT="''${2:-}"

        if [[ ! -z "''${PORT// }" ]]; then
          timeout 2.5s ${pkgs.android-tools}/bin/adb connect "''${IP}:''${PORT}"
          exit ''${?}
        fi

        if [[ "''${IP}" != *"."* ]]; then
          HOST_IP="$(${pkgs.inetutils}/bin/ifconfig | ${pkgs.gnused}/bin/sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p')"
          IP="$(printf '%s\n' "''${HOST_IP}" | awk -v IP="''${IP}" -F'.' '{print $1"."$2"."$3"."IP}')"
          printf 'Only the last octet was provided, assuming %s...\n' "''${IP}"
        fi

        printf 'Scanning for open ports on %s...\n' "''${IP}"
        PORTS="$(${pkgs.nmap}/bin/nmap "''${IP}" -p 30000-50000 | awk '/\/tcp/' | cut -d/ -f1)"

        while IFS= read -r port; do
          if [[ -z "''${port// }" ]]; then
            printf 'No open ports found.\n'
            break
          fi

          printf 'Trying %s:%s...\n' "''${IP}" "''${port}"

          if timeout 2.5s ${pkgs.android-tools}/bin/adb connect "''${IP}:''${port}"; then
            break
          fi
        done <<< "''${PORTS}"
      '';
    in
    "${script}/bin/adbrute";
}
