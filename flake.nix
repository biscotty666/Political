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
            dendextend
            ggrepel
            ggspatial
            gt
            kit
            leaflet
            microbenchmark
            quanteda
            quanteda_textstats
            quanteda_textplots
            readtext
            seededlda
            sf
            smacof
            styler
            tidycensus
            tidyverse
            WDI
            webshot2
            zeallot
            ;
        };
      };
    }) nixpkgs.legacyPackages;
  };
}
