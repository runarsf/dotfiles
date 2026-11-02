_: {
  perSystem =
    { pkgs, ... }:
    {
      packages.zsh-manpage-completion-generator = pkgs.buildGoModule {
        pname = "zsh-manpage-completion-generator";
        version = "1.0.2";

        src = pkgs.fetchFromGitHub {
          owner = "umlx5h";
          repo = "zsh-manpage-completion-generator";
          rev = "v1.0.2";
          hash = "sha256-0CtUafPFt0OxnwdtMSxm/1jcYmDyacj9OoSvfJchixE=";
        };

        vendorHash = "sha256-Wb00v363VjrRKMRQ2beA1pxRYB7LY9yTHPdiXIDdLQA=";

        meta.mainProgram = "zsh-manpage-completion-generator";
      };
    };
}
