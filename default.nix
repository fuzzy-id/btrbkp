{ mkDerivation, base, btrfs, directory, filepath, process, random
, stdenv, tagged, tasty, tasty-hunit, tasty-quickcheck, tasty-th
, temporary, unix
}:
mkDerivation {
  pname = "btrbkp";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base btrfs filepath unix ];
  testHaskellDepends = [
    base btrfs directory filepath process random tagged tasty
    tasty-hunit tasty-quickcheck tasty-th temporary unix
  ];
  description = "Backup routines for BTRFS";
  license = stdenv.lib.licenses.gpl3;
}
