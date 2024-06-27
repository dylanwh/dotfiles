#!/usr/bin/env perl
# vim: set ft=perl sw=4 ts=4 et:
use strict;
use warnings;
use File::Basename;
use File::Path qw(mkpath);
use Cwd qw(realpath);
use Text::ParseWords qw(shellwords);
use Getopt::Long qw(:config gnu_getopt);
use English qw(-no_match_vars $UID $GID);


my $dir = dirname(realpath(__FILE__));

my $build     = 0;
my $run       = 0;
my $image     = basename($dir);
my $container = basename($dir) . '-dev';
my $command   = 'tmux new-session -A -s main';
my $network;
GetOptions(
    'build|b'     => \$build,
    'run|r'       => \$run,
    'container|C' => \$container,
    'image|I=s'   => \$image,
    'network|n=s' => \$network,
    'command|c=s' => \$command,
);

my $dockerfile = "$dir/Dockerfile";
my $content = do { local $/; <DATA> };
open my $fh, '>', $dockerfile or die "Could not open '$dockerfile' $!";
print $fh $content;
close $fh;

my $dockerignore = "$dir/.dockerignore";
open $fh, '>', $dockerignore or die "Could not open '$dockerignore' $!";
print $fh "home\n";
close $fh;

if ($build) {
    my @cmd = ('docker', 'build');
    if ($network) {
        push @cmd, '--network', $network;
    }
    my ($gid) = split(/\s+/, $GID);
    push @cmd, "--build-arg", "UID=$UID", "--build-arg", "GID=$gid";
    push @cmd, '-t', $image, $dir;
    run(@cmd);
}

mkpath("$dir/home") unless -d "$dir/home";

if ($run) {
    run('docker', 'rm', '-fv', $container);
    run('docker', 'run',
        maybe( "--network" => $network ),
        maybe( '--name', $container ),
        maybe( "--hostname" => 'dev' ),
        env(),
        -v => "$dir/home:/home/dylan",
        -v => "/tmp:/host/tmp",
        '-d', '--init', '--rm', '-it',
        $image, "tmux start-server; and sleep infinity"
    );
}


run('docker', 'exec', env(), '-it', $container, shellwords($command));

sub maybe {
    my ($key, $value) = @_;
    return defined($value) ? ($key, $value) : ();
}

sub run {
    print join(' ', @_) . "\n";
     system(@_) == 0
         or die "system @_ failed: $?";
}

sub env {
    my @env;
    foreach my $key (keys %ENV) {
        if ($key =~ /^LC_/) {
            push @env, "-e", "$key=$ENV{$key}";
        }
        elsif ($key eq "TERM" || $key eq "TZ" || $key eq "TERM") {
            push @env, "-e", "$key=$ENV{$key}";
        }
        elsif ($key eq "SSH_AUTH_SOCK") {
            push @env, "-e", "$key=/host/$ENV{$key}";
        }
    }

    return @env;
}

__DATA__
FROM ubuntu:22.04
ARG UID=1000
ARG GID=1000

ENV TZ=US/Pacific
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y \
 && apt-get upgrade -y

RUN apt-get install -y \
    autoconf           \
    bind9-dnsutils     \
    black              \
    build-essential    \
    cmake              \
    curl               \
    emacs-nox          \
    fd-find            \
    fish               \
    git                \
    gron               \
    htop               \
    httpie             \
    hub                \
    iproute2           \
    jq                 \
    jsonnet            \
    libncurses-dev     \
    libssl-dev         \
    locales            \
    moreutils          \
    ncdu               \
    nmap               \
    nq                 \
    pkg-config         \
    protobuf-compiler  \
    pv                 \
    ripgrep            \
    sudo               \
    tmux               \
    tree               \
    whois
ENV UID=$UID
ENV GID=$GID
ENV LANG en_US.UTF-8 
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen

RUN yes | unminimize \
 && apt-get install -y man-db manpages-posix

RUN groupadd -g $GID dylan \
 && useradd -u $UID -g $GID -m -s /usr/bin/fish dylan \
 && echo "dylan ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/dylan

USER dylan
WORKDIR /home/dylan

CMD [ "tmux", "new-session" ]
ENTRYPOINT ["/usr/bin/fish", "-l", "-c"]



