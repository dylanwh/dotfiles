{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
  makeWrapper,
  pi-coding-agent,
}:

let
  version = "0.0.34";
in
buildNpmPackage {
  pname = "pi-acp";
  inherit version;

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "4eade93d5eb1b294695a7ac8f8f44982063bbbef";
    hash = "sha256-QRwxOtTZOY+Np3PkAoy2o2PrUzEqjItM/372sCPlSMo=";
  };

  npmDepsHash = "sha256-BvLNtFfp1cMVjzWcMRSdhTqiJrTfbFoUbWkkPW9200o=";

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/pi-acp \
      --prefix PATH : ${lib.makeBinPath [ pi-coding-agent ]}
  '';

  meta = {
    description = "ACP adapter for the pi coding agent";
    homepage = "https://github.com/svkozak/pi-acp";
    changelog = "https://github.com/svkozak/pi-acp/releases/tag/v${version}";
    license = lib.licenses.mit;
    mainProgram = "pi-acp";
    platforms = lib.platforms.unix;
  };
}
