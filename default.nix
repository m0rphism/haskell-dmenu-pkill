{ mkDerivation, base, containers, directory, dmenu, lens, mtl
, process, stdenv, transformers
}:
mkDerivation {
  pname = "dmenu-pkill";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory dmenu lens mtl process transformers
  ];
  homepage = "https://github.com/m0rphism/haskell-dmenu-pkill";
  description = "dmenu script for killing applications. Can be sorted by process id or CPU/Memory usage.";
  license = stdenv.lib.licenses.bsd3;
}
