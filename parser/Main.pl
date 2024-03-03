#!/usr/bin/perl -I.

use strict;

use utf8;
use lib '.';
use InputStream;
use Tokeniser;
use Parser;
use Data::Dumper;
use JSON;
use Encode;

$Data::Dumper::Indent = 1;

my $input = "
const const 4 = lambda(🔥, b) {
    🔥 + b!
}!
4(1, 2)!
if (;maybe) {
    1!
} else {
    2!
}!
reverse!
const const list = [1, 2, 3]!
";

my $stream = new InputStream($input);
my $tokeniser = new Tokeniser($stream);
# while (my $token = $tokeniser->next()) {
#     print Dumper($token) . "\n";
# }
# exit 0;
my $parser = new Parser($tokeniser);
my $ast = $parser->parse_toplevel();
print Dumper($ast) . "\n";
print to_json($ast) . "\n\n";
