{ mkDerivation, base, btrfs, directory, filepath, lens, process
, random, stdenv, tagged, tasty, tasty-hunit, tasty-quickcheck
, tasty-th, temporary, tinylog, unix
}:
mkDerivation {
  pname = "btrbkp";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base btrfs filepath unix ];
  testHaskellDepends = [
    base btrfs directory filepath lens process random tagged tasty
    tasty-hunit tasty-quickcheck tasty-th temporary tinylog unix
  ];
  description = "Backup routines for BTRFS";
  license = stdenv.lib.licenses.gpl3;
}
