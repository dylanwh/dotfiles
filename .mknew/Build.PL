#!/usr/bin/perl
use strict;
use warnings;
use Module::Build;

my $build = new Module::Build (
    module_name => 'My::Module',
    license     => 'perl',
    requires    => { },
);

$build->create_build_script();
