{ mkDerivation, aeson, aeson-pretty, base, binary, bytestring
, directory, filepath, HTF, HUnit, mtl, optparse-applicative
, parsec, process, shelly, stdenv, text, transformers, unix, zlib
}:
mkDerivation {
  pname = "super-user-spark";
  version = "0.2.0.3";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base binary bytestring directory filepath HTF
    mtl optparse-applicative parsec process shelly text transformers
    unix zlib
  ];
  testHaskellDepends = [
    aeson aeson-pretty base binary bytestring directory filepath HTF
    HUnit mtl optparse-applicative parsec process shelly text
    transformers unix zlib
  ];
  jailbreak = true;
  description = "Configure your dotfile deployment with a DSL";
  license = stdenv.lib.licenses.mit;
}
