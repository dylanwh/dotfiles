#!/usr/bin/env perl
# vim: set ft=perl sw=4 ts=4 et:
use strict;
use warnings;
use Env qw($HOME);
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
my $exec   = undef;
my $help   = 0;
my $network;
GetOptions(
    'build|b'     => \$build,
    'run|r'       => \$run,
    'container|C' => \$container,
    'image|I=s'   => \$image,
    'network|n=s' => \$network,
    'exec|e=s'    => \$exec,
    'help|h'      => \$help,
);

if ($help) {
    print <<~"HELP";
    Usage: $0 [options]

    Options:
        --build, -b       Build the docker image
        --run, -r         Run the docker container
        --container, -C   Name of the container
        --image, -I       Name of the image
        --network, -n     Name of the network
        --exec, -e        Execute a command in the container
        --help, -h        This help message
    HELP
    exit 0;
}

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
        '-P',
        env(),
        -v => "$dir/home:/home/dylan",
        -v => '/var/run/docker.sock:/var/run/docker.sock',
        '-d', '--rm', '-it',
        $image
    );
    # discover which port sshd is listening on
    my $port = `docker port $container 22`;
    if ($port =~ /:(\d+)$/) {
        $port = $1;
        mkpath("$HOME/.ssh/config.d") unless -d "$HOME/.ssh/config.d";
        open $fh, '>', "$HOME/.ssh/config.d/$container" or die "Could not open '$HOME/.ssh/config.d/$container' $!";
        print $fh "Host $container\n";
        print $fh "  ForwardAgent yes\n";
        print $fh "  StrictHostKeyChecking no\n";
        print $fh "  HostName localhost\n";
        print $fh "  Port $port\n";
        print $fh "  User dylan\n";
        close $fh;
    }

}

run('docker', 'exec', env(), '-it', $container, shellwords($exec)) if defined $exec;

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

RUN apt-get update -y  \
 && apt-get upgrade -y \
 && apt-get install -y \
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
    inetutils-ping     \
    iperf3             \
    iproute2           \
    jq                 \
    jsonnet            \
    libncurses-dev     \
    libssl-dev         \
    locales            \
    moreutils          \
    ncdu               \
    net-tools          \
    netcat             \
    nmap               \
    nq                 \
    openssh-server     \
    pkg-config         \
    protobuf-compiler  \
    pv                 \
    ripgrep            \
    s6                 \
    software-properties-common \
    sqlite3            \
    strace             \
    sudo               \
    time               \
    tmux               \
    tree               \
    whois              \
    zstd               \
 && add-apt-repository ppa:neovim-ppa/stable \
 && apt-get update -y \
 && apt-get install -y neovim

ENV UID=$UID
ENV GID=$GID
ENV LANG en_US.UTF-8 
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen \
 && yes | unminimize \
 && apt-get install -y man-db manpages-posix

 # write ssh s6 init script
RUN mkdir -p /etc/s6/sshd /run/sshd \
 && echo '#!/usr/bin/execlineb -P' > /etc/s6/sshd/run \
 && echo '/usr/sbin/sshd -D' >> /etc/s6/sshd/run \
 && chmod +x /etc/s6/sshd/run \
 && ln -s /etc/s6/sshd /etc/s6/sshd

# create dylan user
RUN groupadd -g $GID dylan \
 && useradd -u $UID -g $GID -m -s /usr/bin/fish dylan \
 && echo "dylan ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/dylan

EXPOSE 22

WORKDIR /home/dylan

CMD []

# s6 init
ENTRYPOINT [ "/usr/bin/s6-svscan", "/etc/s6" ]


