package InputStream;

use strict;
use warnings;

use Encode;

# param {string} input
sub new {
    my $class = shift;
    my $self = {
        input => $_[0],
        line => 1,
        col => 0
    };
    bless $self, $class;
    return $self;
}

# return {string} next character
sub next {
    my $self = shift;
    my $char = substr($self->{input}, 0, 1, '');
    if ($char eq "\n") {
        $self->{line}++;
        $self->{col} = 0;
    } else {
        $self->{col}++;
    }
    return $char;
}

# return {string} next character without removing it
sub peek {
    my $self = shift;
    return substr($self->{input}, 0, 1);
}

# return {boolean} true if no more characters
sub eof {
    my $self = shift;
    return length($self->{input}) == 0;
}

sub croak {
    my $self = shift;
    my $msg = shift;
    die "$msg at $self->{line}:$self->{col}";
}

1;
