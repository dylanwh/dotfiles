#!/usr/bin/perl
use strict;
use warnings;
use utf8;
use charnames ":full";
binmode STDOUT, ":encoding(utf8)";
eval qq{print "\\N{@ARGV}\\n"};
