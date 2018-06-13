{ mkDerivation, base, containers, mtl, directory, process, stdenv }:
mkDerivation {
  pname = "HPetriNets";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl directory process ];
  homepage = "https://git.science.uu.nl:r.klomp/HPetriNets";
  description = "EDSL for constructing Petri Nets";
  license = stdenv.lib.licenses.bsd3;
}
