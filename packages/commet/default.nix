_: {
  perSystem = {pkgs, ...}: {
    packages.commet = rec {
      appId = "chat.commet.commetapp";
      sha256 = "";
      bundle = "${pkgs.fetchurl {
        url = "https://github.com/commetchat/commet/releases/download/v0.4.2+hotfix.2/chat.commet.commetapp.flatpak";
        inherit sha256;
      }}";
    };
  };
}
