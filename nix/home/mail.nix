{
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
