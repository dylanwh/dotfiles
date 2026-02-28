{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.git = {
    enable = true;

    settings = {
      user = {
        name = lib.mkDefault "Dylan Hardison";
        email = lib.mkDefault "dylan@hardison.net";
      };
      color = {
        branch = "auto";
        diff = "auto";
        status = "auto";
        ui = "auto";
      };
      core.excludesfile = "~/.cvsignore";
      diff.colorMoved = "default";
      init.defaultBranch = "main";
      pull.ff = "only";
      push = {
        default = "simple";
        autoSetupRemote = true;
      };
      merge.tool = "ediff";
      mergetool.ediff.cmd = ''emacsclient -c -a ""  --eval "(ediff-merge-files-with-ancestor \"$LOCAL\" \"$REMOTE\" \"$BASE\" nil \"$MERGED\")"'';
      github.user = "dylanwh";
      url."https://github.com/".insteadOf = "git://github.com/";
      alias = {
        br = "branches";
        ci = "commit";
        co = "checkout";
        ndiff = "!env DELTA_NAVIGATE=1 git diff";
        reword = "commit --amend";
        st = "status --short --branch";
        top = "!pwd";
        up = "!git fetch --all --tags --prune && git pull --rebase";
      };
    };
  };

  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = config.programs.git.settings.user.name;
        email = config.programs.git.settings.user.email;
      };
    };
  };

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      features = "side-by-side line-numbers decorations";
      whitespace-error-style = "22 reverse";
      decorations = {
        commit-decoration-style = "bold yellow box ul";
        file-style = "bold yellow ul";
        file-decoration-style = "none";
      };
    };
  };
}
