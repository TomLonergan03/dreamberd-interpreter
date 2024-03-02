package Tokeniser;

use strict;
use warnings;
use Set::Object;

my $KEYWORDS = Set::Object->new(("if", "then", "else", "lambda", "true", "false", "reverse"));
my $OPERATORS = qr/[+\-*\/%=&|<>;\^]/;
my $PUNCTUATION = qr/[\(\),!\{\}\[\]]/;

sub new {
    my $class = shift;
    my $self = {
        current => undef,
        input => shift
    };
    bless $self, $class;
    return $self;
}

sub is_keyword {
    my $self = shift;
    my $id = shift;
    return exists $KEYWORDS->{$id};
}

sub is_digit {
    my $self = shift;
    my $char = shift;
    return $char =~ /[0-9]/;
}

sub is_id_start {
    my $self = shift;
    my $char = shift;
    return $char =~ /[a-z_]/i;
}

sub is_id {
    my $self = shift;
    my $char = shift;
    return $self->is_id_start($char) || $self->is_digit($char);
}

sub is_operator {
    my $self = shift;
    my $char = shift;
    return $char =~ $OPERATORS;
}

sub is_punc {
    my $self = shift;
    my $char = shift;
    return $char =~ $PUNCTUATION;
}

sub is_whitespace {
    my $self = shift;
    my $char = shift;
    return $char =~ /\s/;
}

sub read_while {
    my $self = shift;
    my $predicate = shift;
    my $str = '';
    while (!$self->{input}->eof() && $predicate->($self->{input}->peek())) {
        $str .= $self->{input}->next();
    }
    return $str;
}

sub read_number {
    my $self = shift;
    my $number = $self->read_while(sub { $self->is_digit(shift) });
    return { "type" => 'number', "value" => $number };
}

sub read_ident {
    my $self = shift;
    my $id = $self->read_while(sub { $self->is_id(shift) });
    return {
        "type" => $self->is_keyword($id) ? 'keyword' : 'variable',
        "value" => $id
    };
}

sub read_escaped {
    my $self = shift;
    my $start = shift;
    my $escaped = 0;
    my $str = '';
    $self->{input}->next();
    while (!$self->{input}->eof()) {
        my $char = $self->{input}->next();
        if ($escaped) {
            $str .= $char;
            $escaped = 0;
        } elsif ($char eq '\\') {
            $escaped = 1;
        } elsif ($char eq $start) {
            last;
        } else {
            $str .= $char;
        }
    }
    return $str;
}

sub read_string {
    my $self = shift;
    return { "type" => 'string', "value" => $self->read_escaped('"') };
}

sub skip_comment {
    my $self = shift;
    $self->read_while(sub { shift() ne "\n" });
    $self->{input}->next();
}

sub read_next {
    my $self = shift;
    $self->read_while(sub { $self->is_whitespace(shift) });
    if ($self->{input}->eof()) {
        return undef;
    }
    my $char = $self->{input}->peek();
    if ($char eq '#') {
        $self->skip_comment();
        return $self->read_next();
    }
    if ($char eq '"') {
        return $self->read_string();
    }
    if ($self->is_digit($char)) {
        return $self->read_number();
    }
    if ($self->is_id_start($char)) {
        return $self->read_ident();
    }
    if ($self->is_punc($char)) {
        return {
            "type" => 'PUNCTUATION',
            "value" => $self->{input}->next()
        };
    }
    if ($self->is_operator($char)) {
        return {
            "type" => 'operator',
            "value" => $self->read_while(sub { $self->is_operator(shift) })
        };
    }
    $self->{input}->croak("Can't handle character: \"$char\"");
}

sub peek {
    my $self = shift;
    $self->{current} = $self->{current} || $self->read_next();
    return $self->{current};
}

sub next {
    my $self = shift;
    my $token = $self->{current};
    $self->{current} = undef;
    return $token || $self->read_next();
}

sub eof {
    my $self = shift;
    return length ($self->peek() // '') == 0;
}

sub croak {
    my $self = shift;
    my $msg = shift;
    die $msg;
}

1;