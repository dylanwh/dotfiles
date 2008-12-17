#!/usr/bin/perl
use strict;
use warnings;
use YAML::Syck;

my $file = shift or die "usage: $0 contacts.yml\n";
my $contacts = LoadFile($file);

foreach my $key (sort keys %{ $contacts }) {
    my $contact = $contacts->{$key};
    my @tags    = exists $contact->{tags} ? @{ $contact->{tags} } : ();
    my %email   = exists $contact->{email} ? %{ $contact->{email} } : ();
    my $groups  = join(" ", map { "-group $_" } @tags);
    my @labels  = keys %email;

    next if @labels == 0;

    foreach my $label (@labels) {
        my $alias = $label eq 'default' ? $key : "$key.$label";
        printf("alias %s %s \"%s\" <%s>\n", $groups, $alias, $contact->{name}, $email{$label},);
    }
}
