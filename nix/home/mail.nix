{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    isync
    mu
    mutt
  ];

  # keeping notmuch around, but so far I don't really like the UI very much.
  # I think mu4e is much better looking, though notmuch is more powerful.
  #
  # programs.notmuch = {
  #   enable = true;
  #   new.tags = [ "new" ];
  #   hooks.postNew = "afew --tag --new";
  #   extraConfig = {
  #     database.path = "${config.home.homeDirectory}/Mail";
  #     user = {
  #       name = "Dylan Hardison";
  #       primary_email = "dylan@hardison.net";
  #     };
  #     search.exclude_tags = "deleted;spam;";
  #   };
  # };

  # programs.afew = {
  #   enable = true;
  #   extraConfig = ''
  #     [HeaderMatchingFilter.1]
  #     header = X-ME-VSCategory
  #     pattern = (?P<category>.+)
  #     tags = +{category}

  #     [Filter.9]
  #     message = Fallthrough
  #   '';
  # };

  home.sessionVariables = {
    MAILDIR = "$HOME/Mail";
  };

  home.activation = {
    createMailDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ~/Mail
    '';
  };

  home.file.".mbsyncrc".text = ''
    # First section: remote IMAP account
    IMAPAccount fastmail
    Host imap.fastmail.com
    Port 993
    User dylan@hardison.net
    # For simplicity, this is how to read the password from another file.
    # For better security you should use GPG https://gnupg.org/
    PassCmd "cat ~/.fastmail_password"
    TLSType IMAPS
    TLSVersions +1.2 +1.3

    IMAPStore fastmail-remote
    Account fastmail

    # This section describes the local storage
    MaildirStore fastmail-local
    Path ~/Mail/
    Inbox ~/Mail/INBOX
    # The SubFolders option allows to represent all
    # IMAP subfolders as local subfolders
    SubFolders Verbatim

    # This section a "channel", a connection between remote and local
    Channel fastmail
    Far :fastmail-remote:
    Near :fastmail-local:
    Patterns *
    Expunge None
    CopyArrivalDate yes
    Sync All
    Create Near
    SyncState *
  '';

}
