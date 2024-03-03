#!/usr/bin/perl -I.

use strict;

use lib '.';
use InputStream;
use Tokeniser;
use Parser;
use Data::Dumper;

my $input = "5 + 2!";

# "
# sum = lambda(a, b) {
#     a + b!
# }!
# sum(1, 2)!
# reverse!
# ";

my $stream = new InputStream($input);
my $tokeniser = new Tokeniser($stream);
my $parser = new Parser($tokeniser);
print $input . "\n";
print Dumper($parser->parse_toplevel());
