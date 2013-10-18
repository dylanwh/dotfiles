#!/usr/bin/env perl
use strict;
use warnings;
use 5.16.0;
use Path::Class::Rule;
use Config::GitLike;
use YAML::XS;

my $config      = parse(Config::GitLike->load_file('.git/config'), 'submodule');
my $modules     = parse(Config::GitLike->load_file('.gitmodules'), 'submodule');
my $modules_rule = Path::Class::Rule->new->skip_dirs('modules')->file->name('index');
my $modules_iter = sub {
    state $iter = $modules_rule->iter( '.git/modules' ); 
    my $file = $iter->();
    return $file->dir->relative('.git/modules') if $file;
    return undef;
};

say "-- checking .git/modules dir";
while (my $path = $modules_iter->()) {
    unless ($modules->{$path}) {
        say "UM $path";
    }
    unless ($config->{$path}) {
        say "UC $path";
    }
}

say "-- checking .git/config";
for my $path (keys %$config) {
    unless (-d $path) {
        say "EC $path";
    }
    unless (-f "$path/.git") {
        say "FC $path" if -d $path;
    }

    unless (exists $modules->{$path}) {
        say "RC $path";
    }
}

say "-- checking .gitmodules";
for my $path (keys %$modules) {
    unless (-d $path) {
        say "EM $path";
    }
    unless (-f "$path/.git") {
        say "FM $path" if -d $path;
    }
    unless ($path eq $modules->{$path}{path}) {
        say "PM $path $modules->{$path}{path}";
    }
    unless (exists $config->{$path}) {
        say "RM $path";
    }
}


sub parse {
    my ($hash, $prefix) = @_;
    my $qprefix = quotemeta $prefix;
    my %result;

    for (grep { /^$qprefix\./ } keys %$hash) {
        my ($section, $key) = /^$qprefix\.(.*?)\.(\w+)$/;
        $result{$section}{$key}    = $hash->{$_};
        $result{$section}{KEY}     = $_;
        $result{$section}{SECTION} = "$prefix.$section";
    }

    return \%result;
}
