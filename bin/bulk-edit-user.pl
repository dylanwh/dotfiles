#!/Users/dylan/.plenv/shims/perl
use strict;
use warnings FATAL => 'all';
use 5.10.0;
$| = 1;

# opens editusers.cgi for the specified email addresses.  this avoids the need
# to search for the email address to find the user_id, then click to edit, and
# is most useful when editing multiple users.
#
# syntax:
#   bulk-user-edit
#       without args, you'll be prompted to paste lines which contain the user
#       to edit.
#   bulk-user-edit [dev|stage]
#       if you specify "dev" or "stage" as an arg, the development or staging
#       servers will be used instead of production.
#   bulk-user-edit login@example.com
#       passing one or more address on the command line will work on just those
#       addresses.
use FindBin;
use lib $FindBin::Bin;
use JSON;
use List::MoreUtils 'uniq';
use LWP::UserAgent;
use HTTP::Request;
use URI::Escape;
use Sys::Hostname;

my $url_base = 'https://bugzilla.mozilla.org';
$url_base = 'https://bugzilla-dev.allizom.org' if grep { /\bdev\b/ } @ARGV;
$url_base = 'https://bugzilla.allizom.org' if grep { /\bstage\b/ } @ARGV;

my @list;
foreach my $arg (@ARGV) {
    push @list, $arg if $arg =~ /\@/;
}
if (!@list) {
    undef @ARGV;
    print "paste lines with email addresses, ^D to finish\n";
    @list = <>;
    chomp(@list);
}

my @tests = (
    '<([^>]+)>',
    '\(([^\)]+)\)',
    '\[([^\]]+)\]',
    '\b(\S+\@.+\..+)$',
);

my @logins;
foreach (@list) {
    s/(^\s+|\s+$)//g;
    next if $_ eq '';
    next unless /\@/;

    TEST: foreach my $test (@tests) {
        while ($_ =~ /$test/g) {
            my $login = $1;
            $login =~ s/^(\S+)\s.*/$1/;
            $login =~ s/^mailto://i;
            $login =~ s/^[,\.\(\[\<]+//;
            $login =~ s/[,\.\)\]\>]+$//;
            next if $login !~ /^\S+\@\S+/ || $login =~ /\s/;
            push @logins, $login;
            print "<$login>\n";
            last TEST;
        }
    }
}
die "no logins found\n" unless @logins;
@logins = map { lc } uniq sort @logins;

print "\nchecking " . scalar(@logins) . " user" . (scalar(@logins) == 1 ? '' : 's') . "\n";

my ($code, $users) = get_users(@logins);
if ($code == 400) {
    foreach my $login (@logins) {
        my (undef, $user) = get_users($login);
        next unless $user;
        push @$users, $user->[0];
    }
}

my @urls;
foreach my $login (@logins) {
    my $url;
    foreach my $rh (@$users) {
        next unless lc($rh->{name}) eq $login;
        $url = "$url_base/editusers.cgi?action=edit&userid=" . $rh->{id};
        last;
    }
    if (!$url) {
        print "failed to find login $login\n";
    } else {
        push @urls, $url;
    }
}
exit unless @urls;

print "\nopening url" . (scalar(@urls) == 1 ? '' : 's') . "..\n";
while (my $url = pop @urls) {
    if (hostname() eq 'bz.glob.com.au') {
        system qq#ssh byron\@mac "open '$url'"#;
    } else {
        system "open '$url'";
    }
    sleep(1) if @urls;
}

sub get_users {
    my (@logins) = @_;
    state $ua //= LWP::UserAgent->new();
    my $request = HTTP::Request->new(GET => $url_base . '/rest/user?include_fields=id,name&names=' . join('&names=', map { uri_escape($_) } @logins));
    my $response = $ua->request($request);

    my $code = $response->code;
    my $users;

    my $data;
    eval {
        $data = decode_json($response->decoded_content);
    };
    die "malform response ($@):\n" . $response->message . "\n" if $@;

    if ($code == 200) {
        $users = $data->{users};
    }
    return ($code, $users);
}
