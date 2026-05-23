_: {
  perSystem = {pkgs, ...}: {
    packages.stderred = pkgs.stderred.overrideAttrs {
      src = pkgs.fetchFromGitHub {
        owner = "sickill";
        repo = "stderred";
        rev = "76fde071dd17a72700098617ae0813848eeba7a3";
        sha256 = "15rwgk782b1gdq857f5d9cn61j0scxxfwbw7qxlqb4whmhqnzh22";
      };
    };
  };
}
