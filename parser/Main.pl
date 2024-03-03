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

my $input = "";
while (<>) {
    $input .= $_;
}

my $stream = new InputStream($input);
my $tokeniser = new Tokeniser($stream);
my $parser = new Parser($tokeniser);
my $ast = $parser->parse_toplevel();
print Dumper($ast) . "\n";
print to_json($ast) . "\n\n";

my $path = "../ast.json";
open my $fh, ">", $path or die "Can't open $path: $!";
print $fh to_json($ast);
close $fh;