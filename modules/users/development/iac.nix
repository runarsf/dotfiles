{
  config,
  outputs,
  pkgs,
  ...
}: let
  cfg = config.modules.dev.iac;
in
  outputs.lib.mkModule config ["dev" "iac"] {
    options' = with outputs.lib; {
      openstack = mkEnableOption "Enable OpenStack-related IaC tools";
    };

    config = {
      home.packages = with pkgs;
        [
          opentofu
          terraform
        ]
        ++ outputs.lib.optionals cfg.openstack [
          openstackclient
        ];
    };
  }
