#!/usr/bin/env perl
use 5.20.3;
use Perl::Tidy;
use Template::Parser;

local $/ = undef;
my $tt_code = <ARGV>;

my $parser    = Template::Parser->new();
my $result    = $parser->parse($tt_code);
my $perl_code = 'my $code = ' . $result->{BLOCK} . ";";
my $output    = "";
Perl::Tidy::perltidy(
    argv        => perltidyrc(),
    source      => \$perl_code,
    destination => \$output
);

$output =~ s/#line \d+ "[^\n"]+"\n//gsm;
$output =~ s/\n+/\n/gsm;

say $output;

sub perltidyrc {
    return [
        "-l=78",
        "-i=4",
        "-ci=4",
        "-vt=2",
        "-cti=0",
        "-pt=1",
        "-bt=1",
        "-sbt=1",
        "-bbt=1",
        "-nsfs",
        "-nolq",
"-wbb=% + - * / x != == >= <= =~ !~ < > | & = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=",
        "-w",
        "-iob",
        "-l=120",
        "-vmll",
        "-ibc",
        "-iscl",
        "-hsc",
        "-mbl=2",
        "-i=2",
        "-ci=2",
        "-vt=0",
        "-pt=2",
        "-bt=2",
        "-sbt=2",
        "-wn",
        "-isbc",
    ];
}
