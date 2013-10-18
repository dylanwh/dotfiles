#!/usr/bin/env perl
use strict;
use warnings;
use 5.18.0;
use Path::Class::Rule;
use Config::GitLike;
use YAML::XS;

my $config  = parse(Config::GitLike->load_file('~/.git/config'), 'submodule');
my $modules = parse(Config::GitLike->load_file('~/.gitmodules'), 'submodule');

for my $path (keys %$config) {
    unless (exists $modules->{$path}) {
        warn '$path does not exist in ~/.gitmodules\n';
        print "git config --remove-section $config->{$path}{SECTION}\n";
        next;
    }
    unless ($modules->{$path}{url} eq $config->{$path}{url}) {
        warn '$path url disagrees between .gitmodules and .git/config\n';
    }
}

for my $path (keys %$modules) {
    unless (exists $config->{$path}) {
        warn '$path does not exist in ~/.git/config\n';
        next;
    }
    unless ($path eq $modules->{$path}{path}) {
        warn '$path inconsistent in .gitmodules\n';
    }
    unless ($modules->{$path}{url} eq $config->{$path}{url}) {
        warn '$path url disagrees between .gitmodules and .git/config\n';
    }
}

sub parse {
    my ($hash, $prefix) = @_;
    my $qprefix = quotemeta $prefix;
    my %result;

    for (grep { /^$qprefix\./ } keys %$hash) {
        my ($section, $key) = /^$qprefix\.(.*?)\.(\w+)$/;
        $result{$section}{$key} = $hash->{$_};
        $result{$section}{KEY}  = $_;
        $result{$section}{SECTION} = "$prefix.$section";
    }

    return \%result;
}
