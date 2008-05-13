#!/usr/bin/perl
use strict;
use warnings;
use YAML::Syck;
use IO::File;
my $file = "$ENV{HOME}/pim/contacts";

local $/ = undef;
my $fh = new IO::File($file) or die "Cannot read $file: $!";
my @book = Load($fh->getline);
my @alias;

foreach my $entry (@book) {
	my $email = $entry->{email} or next;
	my $nick  = $entry->{nick}  or next;
	my $name  = $entry->{name}  or next;
	my $group = $entry->{group} || [];

	next unless $entry->{nick};
	next unless $entry->{name};

	unless (ref $email) {
		$email = { primary => $email };
	}
	unless (ref $group) {
		$group = [$group];
	}

	foreach my $key (keys %$email) {
		my $nick = $key eq 'primary' ? $entry->{nick} : "$entry->{nick}.$key";
		push @alias, {
			nick => $nick,
			name => $name,
			email => $email->{$key},
			groups => join(' ', map("-group $_", @$group)),
		};
	}
}

print qq{alias $_->{groups} $_->{nick} "$_->{name}" <$_->{email}>\n} foreach @alias;


