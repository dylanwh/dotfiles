#!/usr/bin/env perl
use strict;
use warnings;

my @hosts;
my %seen;

# Parse ~/.ssh/config.d/* for Host directives
my @config_files = glob("$ENV{HOME}/.ssh/config.d/*");
for my $file (@config_files) {
    open my $fh, '<', $file or next;
    while (<$fh>) {
        next unless /^\s*Host\s+(.+)/i;
        for my $host (split /\s+/, $1) {
            next if $host =~ /[*?]/;
            $seen{$host}++ or push @hosts, $host;
        }
    }
}

# Parse ~/.ssh/known_hosts* for hostnames
my @known_hosts_files = glob("$ENV{HOME}/.ssh/known_hosts*");
for my $file (@known_hosts_files) {
    open my $fh, '<', $file or next;
    while (<$fh>) {
        next if /^\s*#/ || /^\s*$/;
        my ($hostfield) = split /\s+/, $_, 2;
        next unless defined $hostfield;
        next if $hostfield =~ /^\|/;
        for my $entry (split /,/, $hostfield) {
            $entry =~ s/^\[(.+?)\]:\d+$/$1/;
            next if $entry =~ /^[\d.:]+$/;
            $seen{$entry}++ or push @hosts, $entry;
        }
    }
}

# Collect CanonicalDomains from config
my @canonical_domains;
for my $file (@config_files) {
    open my $fh, '<', $file or next;
    while (<$fh>) {
        next unless /^\s*CanonicalDomains\s+(.+)/i;
        push @canonical_domains, split /\s+/, $1;
    }
}

# Strip canonical domain suffixes, preferring short names
for my $host (@hosts) {
    my $dominated;
    for my $domain (@canonical_domains) {
        if ($host =~ /^(.+)\.\Q$domain\E$/) {
            $dominated = $1;
            last;
        }
    }
    if (defined $dominated) {
        next if $seen{$dominated};
        print "$dominated\n";
        $seen{$dominated}++;
    } else {
        print "$host\n";
    }
}
