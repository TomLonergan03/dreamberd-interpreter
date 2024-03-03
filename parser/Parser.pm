package Parser;

use strict;
use warnings;

use utf8;
use JSON;
use Data::Dumper;

$Data::Dumper::Indent = 1;

my $PRECEDENCE = {
    "=" => 1,
    "||" => 2,
    "&&" => 3,
    "<" => 7,
    ">" => 7,
    "<=" => 7,
    ">=" => 7,
    "==" => 7,
    "!=" => 7,
    "+" => 10,
    "-" => 10,
    "*" => 20,
    "/" => 20,
    "%" => 20
};

sub new {
    my $class = shift;
    my $self = {
        input => shift
    };
    bless $self, $class;
    return $self;
}

sub is_punctuation {
    my $self = shift;
    my $char = shift;
    my $token = $self->{input}->peek();
    return $token && $token->{type} eq "punctuation" && (!$char || $token->{value} eq $char) && $token;
}

sub is_keyword {
    my $self = shift;
    my $word = shift;
    my $token = $self->{input}->peek();
    return $token && $token->{type} eq "keyword" && (!$word || $token->{value} eq $word) && $token;
}

sub is_operation {
    my $self = shift;
    my $op = shift;
    my $token = $self->{input}->peek();
    return $token && $token->{type} eq "operator" && (!$op || $token->{value} eq $op) && $token;
}

sub skip_punctuation {
    my $self = shift;
    my $char = shift;
    if ($self->is_punctuation($char)) {
        $self->{input}->next();
    } else {
        $self->{input}->croak("Expecting punctuation: \"$char\"");
    }
}

sub skip_keyword {
    my $self = shift;
    my $word = shift;
    if ($self->is_keyword($word)) {
        $self->{input}->next();
    } else {
        $self->{input}->croak("Expecting keyword: \"$word\"");
    }
}

sub skip_operation {
    my $self = shift;
    my $op = shift;
    if ($self->is_operation($op)) {
        $self->{input}->next();
    } else {
        $self->{input}->croak("Expecting operator: \"$op\"");
    }
}

sub unexpected {
    my $self = shift;
    $self->{input}->croak("Unexpected token: " . encode_json $self->{input}->peek());
}

sub maybe_binary {
    my $self = shift;
    my $left = shift;
    my $my_precedence = shift;
    my $token = $self->is_operation();
    if ($token) {
        my $his_precedence = $PRECEDENCE->{$token->{value}};
        if ($his_precedence > $my_precedence) {
            $self->{input}->next();
            return $self->maybe_binary({
                type => $token->{value} eq "=" ? "assign" : "binary",
                operator => $token->{value},
                left => $left,
                right => $self->maybe_binary($self->parse_atom(), $his_precedence)
            }, $my_precedence);
        }
    }
    return $left;
}

sub delimited {
    my $self = shift;
    my $start = shift;
    my $stop = shift;
    my $separator = shift;
    my $parser = shift;
    my $a = [];
    my $first = 1;
    $self->skip_punctuation($start);
    while (!$self->{input}->eof()) {
        if ($self->is_punctuation($stop)) {
            last;
        }
        if ($first) {
            $first = 0;
        } else {
            $self->skip_punctuation($separator);
        }
        if ($self->is_punctuation($stop)) {
            last;
        }
        push @$a, $parser->();
    }
    $self->skip_punctuation($stop);
    return $a;
}

sub parse_call {
    my $self = shift;
    my $func = shift;
    return {
        type => "call",
        func => $func,
        args => $self->delimited("(", ")", ",", sub { $self->parse_expression() })
    };
}

sub parse_varname {
    my $self = shift;
    my $name = $self->{input}->next();
    if ($name->{type} ne "variable") {
        $self->{input}->croak("Expecting variable name");
    }
    return $name->{value};
}

sub parse_if {
    my $self = shift;
    $self->skip_keyword("if");
    my $condition = $self->parse_expression();
    if (!$self->is_punctuation("{")) {
        $self->skip_keyword("then");
    }
    my $then = $self->parse_expression();
    my $ret = {
        type => "if",
        cond => $condition,
        then => $then
    };
    if ($self->is_keyword("else")) {
        $self->{input}->next();
        $ret->{else} = $self->parse_expression()
    }
    return $ret;
}

sub parse_lambda {
    my $self = shift;
    return {
        type => "lambda",
        vars => $self->delimited("(", ")", ",", sub { $self->parse_varname() }),
        body => $self->parse_expression()
    };
}

sub parse_bool {
    my $self = shift;
    my $token = $self->{input}->next();
    return {
        type => "bool",
        value => $token->{value}
    };
}

sub maybe_call {
    my $self = shift;
    my $expr = shift->();
    $expr = $self->parse_call($expr) if $self->is_punctuation("(");
    return $expr;
}

sub is_binding {
    my $self = shift;
    my $token = $self->{input}->peek();
    return $token && $token->{type} eq "binding";
}

sub parse_binding {
    my $self = shift;
    my $binding = $self->{input}->next();
    $binding->{name} = $self->parse_varname();
    $self->skip_punctuation("=");
    $binding->{value} = $self->parse_expression();
    return $binding;
}

sub parse_array {
    my $self = shift;
    my $values = $self->delimited("[", "]", ",", sub { $self->parse_expression() });
    my $array = {};
    for (my $i = 0; $i < @$values; $i++) {
        $array->{$i} = $values->[$i];
    }
    return {
        type => "array",
        value => $array
    };
}

sub parse_atom {
    my $self = shift;
    return $self->maybe_call(sub {
        if ($self->is_punctuation("(")) {
            $self->{input}->next();
            my $exp = $self->parse_expression();
            $self->skip_punctuation(")");
            return $exp;
        }
        if ($self->is_punctuation("{")) {
            return $self->parse_program();
        }
        if ($self->is_punctuation("[")) {
            return $self->parse_array();
        }
        if ($self->is_operation(";")) {
            $self->{input}->next();
            return { type => "not", body => $self->parse_atom() };
        }
        if ($self->is_binding()) {
            return $self->parse_binding();
        }
        if ($self->is_keyword("if")) {
            return $self->parse_if();
        }
        if ($self->is_keyword("true") || $self->is_keyword("false") || $self->is_keyword("maybe")) {
            return $self->parse_bool();
        }
        if ($self->is_keyword("lambda")) {
            $self->{input}->next();
            return $self->parse_lambda();
        }
        my $token = $self->{input}->next();
        if ($token->{type} eq "variable" || $token->{type} eq "number" || $token->{type} eq "string" || $token->{type} eq "keyword") {
            return $token;
        }
        $self->unexpected();
    });
}

sub parse_toplevel {
    my $self = shift;
    my $program = [];
    while (!$self->{input}->eof()) {
        push @$program, $self->parse_expression();
        if (!$self->{input}->eof()) {
            $self->skip_punctuation("!");
        }
    }
    return { type => "program", program => $program };
}

sub parse_program {
    my $self = shift;
    my $program = $self->delimited("{", "}", "!", sub { $self->parse_expression() });
    if (@$program == 0) {
        return { type => "bool", value => "false" };
    }
    if (@$program == 1) {
        return $program->[0];
    }
    return { type => "program", program => $program };
}

sub parse_expression {
    my $self = shift;
    return $self->maybe_call(sub {
        $self->maybe_binary($self->parse_atom(), 0);
    });
}
