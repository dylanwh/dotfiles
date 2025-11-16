final: prev:
let
  testPython = final.python3.withPackages (ps: [ ps.pexpect ]);
in
{
  fish = prev.fish.overrideAttrs (oldAttrs: {
    preConfigure = ''
      patchShebangs ./build_tools/git_version_gen.sh
      patchShebangs ./tests/test_driver.py
      substituteInPlace ./tests/test_driver.py --replace-warn '"python3",' '"${testPython}/bin/python3",'
    ''
    + final.lib.optionalString (final.stdenv.hostPlatform != final.stdenv.buildPlatform) ''
      export CMAKE_PREFIX_PATH=
    '';

    nativeCheckInputs = builtins.map (
      input: if input == final.python3.withPackages (ps: [ ps.pexpect ]) then testPython else input
    ) (oldAttrs.nativeCheckInputs or [ ]);
  });
}
