{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, process, stdenv
      }:
      mkDerivation {
        pname = "HPetriNets";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base containers directory process ];
        homepage = "https://git.science.uu.nl:r.klomp/HPetriNets";
        description = "EDSL for constructing Petri Nets";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
