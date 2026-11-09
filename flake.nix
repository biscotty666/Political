{
  description = "A basic flake providing a devShell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { nixpkgs, ... }: {
    devShells = builtins.mapAttrs (system: pkgs: {
      default = pkgs.mkShell {
        packages = builtins.attrValues {
          inherit (pkgs)
            R
            quarto
            chromium
            pandoc
            rstudio
            texliveMedium
            ;
          inherit (pkgs.rPackages)
            cartogram
            codetools
            crsuggest
            collapse
            ggspatial
            gt
            kit
            leaflet
            microbenchmark
            sf
            styler
            tidycensus
            tidyverse
            WDI
            zeallot
            ;
        };
      };
    }) nixpkgs.legacyPackages;
  };
}
