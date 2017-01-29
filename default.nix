{ mkDerivation, attoparsec, base, btrfs, directory, filepath
, hourglass, ini, lens, mtl, optparse-applicative, process, random
, stdenv, tagged, tasty, tasty-hunit, tasty-quickcheck, tasty-th
, temporary, text, tinylog, transformers, unix
, unordered-containers, uuid
}:
mkDerivation {
  pname = "btrbkp";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base btrfs filepath hourglass ini lens mtl
    optparse-applicative process text tinylog transformers unix uuid
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    attoparsec base btrfs directory filepath hourglass ini lens mtl
    process random tagged tasty tasty-hunit tasty-quickcheck tasty-th
    temporary text tinylog unix unordered-containers
  ];
  description = "Backup routines for BTRFS";
  license = stdenv.lib.licenses.gpl3;
}
