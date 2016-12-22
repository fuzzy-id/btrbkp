{ mkDerivation, base, btrfs, directory, filepath, hourglass, ini
, lens, mtl, optparse-applicative, process, random, stdenv, tagged
, tasty, tasty-hunit, tasty-quickcheck, tasty-th, temporary
, tinylog, transformers, unix, unordered-containers
}:
mkDerivation {
  pname = "btrbkp";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base btrfs filepath hourglass lens mtl optparse-applicative tinylog
    transformers unix
  ];
  testHaskellDepends = [
    base btrfs directory filepath hourglass ini lens mtl process random
    tagged tasty tasty-hunit tasty-quickcheck tasty-th temporary
    tinylog unix unordered-containers
  ];
  description = "Backup routines for BTRFS";
  license = stdenv.lib.licenses.gpl3;
}
