{ config, outputs, ... }:

outputs.lib.mkDesktopModule config "trayer" {
  services.trayer = {
    enable = true;
    settings = {
      edge = "top";
      align = "right";
      SetDockType = true;
      SetPartialStrut = false;
      expand = true;
      widthtype = "request";
      transparent = true;
      alpha = 0;
      tint = "0x0D1117";
      height = 30;
      heighttype = "pixel";
      monitor = "primary";
      margin = 20;
      distance = 11;
      padding = 0;
    };
  };
}
