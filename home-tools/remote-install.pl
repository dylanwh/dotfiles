use strict;
use warnings;

my $expected_arch = $ARGV[0] // die "missing arch\n";
chomp(my $remote_arch = `uname -m`);
if ($remote_arch ne $expected_arch) {
    die "Error: built for $expected_arch, but this host is $remote_arch (rebuild with --argstr arch $remote_arch)\n";
}

open(my $tar, "|-", "tar", "-C", $ENV{HOME}, "-xzf", "-") or die "tar: $!\n";
binmode(DATA);
binmode($tar);
while (read(DATA, my $buf, 65536)) {
    print {$tar} $buf;
}
close($tar) or die "tar failed ($?)\n";

my $bashrc = "$ENV{HOME}/.bashrc";
my $content = "";
if (-e $bashrc) {
    open(my $in, "<", $bashrc) or die "open $bashrc: $!\n";
    local $/;
    $content = <$in>;
    close($in);
}

my $changed = 0;
if ($content !~ /^# home-tools PATH$/m) {
    $content = qq{# home-tools PATH\nif [[ \$- != *i* ]]; then export PATH="\$HOME/.local/bin:\$PATH"; fi\n\n} . $content;
    $changed = 1;
}
if ($content !~ m{\.local/share/home-tools/bashrc}) {
    $content .= qq{\n[ -f "\$HOME/.local/share/home-tools/bashrc" ] && . "\$HOME/.local/share/home-tools/bashrc"\n};
    $changed = 1;
}
if ($changed) {
    open(my $out, ">", $bashrc) or die "write $bashrc: $!\n";
    print {$out} $content;
    close($out);
}

$ENV{PATH} = "$ENV{HOME}/.local/bin:$ENV{PATH}";
print "---\n";
print "rsync: " . (`rsync --version` =~ /^([^\n]+)/)[0] . "\n";
print "rg: " . (`rg --version` =~ /^([^\n]+)/)[0] . "\n";
print "fd: " . `fd --version`;
print "jq: " . `jq --version`;
print "less: " . (`less --version` =~ /^([^\n]+)/)[0] . "\n";
print "starship: " . (`starship --version` =~ /^([^\n]+)/)[0] . "\n";
print "terminfo: xterm-ghostty ok\n" if -e "$ENV{HOME}/.terminfo/x/xterm-ghostty";
print "Done!\n";
__DATA__
