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
my $container = basename($dir) . '-dev';
my $exec      = undef;
my $image     = basename($dir);
my $network;
my $run        = 0;
my $privileged = 0;
my $help       = 0;

GetOptions(
  'build|b'      => \$build,
  'container|C'  => \$container,
  'exec|e=s'     => \$exec,
  'image|I=s'    => \$image,
  'network|n=s'  => \$network,
  'privileged|p' => \$privileged,
  'run|r'        => \$run,
  'help|h'       => \$help,
);

if ($help) {
  print <<~"HELP";
    Usage: $0 [options]

    Options:
        --build, -b       Build the docker image
        --container, -C   Name of the container
        --exec, -e        Execute a command in the container
        --image, -I       Name of the image
        --network, -n     Name of the network
        --privileged, -p  Run the container in privileged mode
        --run, -r         Run the docker container
        --help, -h        This help message
    HELP
  exit 0;
}

my $dockerfile = "$dir/Dockerfile";
my $content    = do { local $/; <DATA> };
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
  # find the gid of the docker group
  my $docker_gid = getgrnam('docker');
  if (defined $docker_gid) {
    push @cmd, "--build-arg", "DOCKER_GID=$docker_gid";
  }

  push @cmd, '-t', $image, $dir;
  run(@cmd);
}

mkpath("$dir/home") unless -d "$dir/home";

if ($run) {
  system('docker', 'rm', '-fv', $container);
  my @run = ('docker', 'run');

  push @run, '--network'  => $network if defined $network;
  push @run, '--name'     => $container;
  push @run, '--hostname' => 'dev';
  push @run, '-P';    # publish all exposed ports to random ports
  push @run, env();
  push @run, -v => "$dir/home:/home/dylan";
  push @run, -v => "$HOME/.ssh:/host/.ssh";
  push @run, -v => "$HOME/.ssh/config.d:/home/dylan/.ssh/config.d" if $privileged;
  push @run, -v => '/var/run/docker.sock:/var/run/docker.sock' if $privileged;
  push @run, '--privileged' if $privileged;
  push @run, '-d', '--rm', '-it', $image;
  run(@run);

  # discover which port sshd is listening on
  my $port = `docker port $container 22`;
  if ($port =~ /:(\d+)$/) {
    $port = $1;
    mkpath("$HOME/.ssh/config.d") unless -d "$HOME/.ssh/config.d";
    open $fh, '>', "$HOME/.ssh/config.d/$container"
      or die "Could not open '$HOME/.ssh/config.d/$container' $!";
    print $fh "Host $container\n";
    print $fh "  ForwardAgent yes\n";
    print $fh "  StrictHostKeyChecking no\n";
    print $fh "  HostName localhost\n";
    print $fh "  Port $port\n";
    print $fh "  User dylan\n";
    close $fh;
  }

}

run('docker', 'exec', env(), '-it', $container, shellwords($exec))
  if defined $exec;

sub maybe {
  my ($key, $value) = @_;
  return defined($value) ? ($key, $value) : ();
}

sub run {
  print join(' ', @_) . "\n";
  system(@_) == 0 or die "system @_ failed: $?";
}

# env returns a list of environment variables to pass to the docker container on
# run.
sub env {
  my @env;
  foreach my $key (keys %ENV) {
    if ($key =~ /^LC_/ || $key eq 'TERM' || $key eq 'TZ' || $key eq 'TERM') {
      push @env, '-e', "$key=$ENV{$key}";
    } elsif ($key eq 'SSH_AUTH_SOCK') {
      push @env, '-e', "$key=/host/$ENV{$key}";
    }
  }

  return @env;
}

__DATA__
FROM ubuntu:24.04
ARG UID=1000
ARG GID=1000
ARG DOCKER_GID

ENV TZ=US/Pacific
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y  \
 && apt-get upgrade -y \
 && apt-get install -y \
    2to3              \
    autoconf          \
    bc                \
    bind9-dnsutils    \
    black             \
    brotli            \
    build-essential   \
    cmake             \
    curl              \
    dc                \
    docker.io         \
    emacs-nox         \
    fd-find           \
    fdisk             \
    fio               \
    fish              \
    git               \
    gron              \
    htop              \
    httpie            \
    hub               \
    inetutils-ping    \
    iperf3            \
    iproute2          \
    jq                \
    jsonnet           \
    libbrotli-dev     \
    libbsd-dev        \
    libbz2-dev        \
    libffi-dev        \
    libgeoip-dev      \
    libsqlite3-dev    \
    libssl-dev        \
    libtool           \
    libtool-bin       \
    libudns-dev       \
    libvterm-dev      \
    lld               \
    locales           \
    moreutils         \
    musl-tools        \
    ncdu              \
    neovim            \
    net-tools         \
    netcat-openbsd    \
    nmap              \
    nq                \
    openssh-server    \
    pigz              \
    pkg-config        \
    protobuf-compiler \
    pv                \
    ripgrep           \
    rsync             \
    s6                \
    sccache           \
    shellcheck        \
    skopeo            \
    sqlite3           \
    strace            \
    sudo              \
    time              \
    tmux              \
    trash-cli         \
    tree              \
    unminimize        \
    unzip             \
    valgrind          \
    whois             \
    wireguard         \
    xattr             \
    zip               \
    zstd

ENV UID=$UID
ENV GID=$GID
ENV DOCKER_GID=$DOCKER_GID
ENV LANG en_US.UTF-8 
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen \
 && yes | unminimize \
 && apt-get install -y man-db manpages-posix

RUN install -m 0755 -d /etc/apt/keyrings \
 && curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc 2>/dev/null \
 && chmod a+r /etc/apt/keyrings/docker.asc

RUN test -f /etc/apt/keyrings/docker.asc

RUN echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo "$UBUNTU_CODENAME") stable" > /etc/apt/sources.list.d/docker.list
RUN test -f /etc/apt/sources.list.d/docker.list
RUN apt-get update
RUN apt-get install -y docker-ce-cli

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

# add dylan to docker group
# Create docker group with same gid as host docker group
RUN if [ -n "$DOCKER_GID" ]; then \
    groupdel docker; \
    groupadd -g $DOCKER_GID docker; \
    gpasswd -a dylan docker; \
fi

EXPOSE 22

WORKDIR /home/dylan

CMD []

# s6 init
ENTRYPOINT [ "/usr/bin/s6-svscan", "/etc/s6" ]


