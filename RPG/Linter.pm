use strict;
use warnings;
use v5.16;

use Data::Dumper;
use Exporter;
use JSON;

my $DCL_DS = 'dcl-ds';
my $DCL_S = 'dcl-s';
my $DCL_C = 'dcl-c';
my $DCL_SUBF = 'dcl-subf';

my $CALC_IDENT = 'ident';
my $CALC_SUBF = 'subf';
my $CALC_IND = 'ind';
my $CALC_OPCODE = 'opcode';
my $CALC_EXSR = 'exsr';

my $C_WARN = -t 1 ? "\033[1;35m" : '';
my $C_NOTE = -t 1 ? "\033[1;36m" : '';
my $C_RESET = -t 1 ? "\033[0m" : '';

my $LINT_NOTE = "note";
my $LINT_WARN = "warning";

my $RULES_GLOBAL = "global";
my $RULES_SHADOW = "shadow";
my $RULES_QUALIFIED = "qualified";
my $RULES_UPPERCASE_CONSTANT = "uppercase-constant";
my $RULES_UNDEFINED_REFERENCE = "undefined-reference";
my $RULES_SUBROUTINE = "subroutine";
my $RULES_UPPERCASE_INDICATOR = "uppercase-indicator";
my $RULES_INDICATOR = "indicator";
my $RULES_UNUSED_VARIABLE = "unused-variable";
my $RULES_REDEFINING_SYMBOL = "redefining-symbol";
my $RULES_UNREACHABLE_CODE = "unreachable-code";
my $RULES_SAME_CASING = "same-casing";

my $default_rules = {
  global => 0,
  shadow => 0,
  qualified => 0,
  'uppercase-constant' => 0,
  'undefined-reference' => 0,
  subroutine => 0,
  'uppercase-indicator' => 0,
  indicator => 0,
  'unused-variable' => 0,
  'redefining-symbol' => 0,
  'unreachable-code' => 0,
  'same-casing' => 0
};

sub sorterrors($$)
{
  my ($a, $b) = @_;
  my ($al, $bl) = ($a->{linksto}, $b->{linksto});

  # place linked notes after the warnings
  my $ano = defined $al ? $al->{lineno} + 0.1 : $a->{lineno};
  my $bno = defined $bl ? $bl->{lineno} + 0.1 : $b->{lineno};

  return $ano <=> $bno;
}

# utility function to loop over each scope,
# the provided subroutine will be called with the scopes.
# i.e the first call will be for the global scope with $scope as the argument,
# the preceding calls with be with '($proc, $scope)' as arguments
sub loopscopes
{
  my ($scope, $sub) = @_;

  $sub->($scope);

  for my $procname (keys %{$scope->{procedures}}) {
    my $proc = $scope->{procedures}->{$procname};
    $sub->($proc, $scope);
  }

  return 1;
}

# returns a hash of all declarations in the current scope,
# calls $callback is a previous declaration is already defined
sub declhash
{
  my ($decls, $scopes, $callback, $adddsdecl) = @_;
  my ($scope) = @{$scopes};

  my $adddecl = sub {
    my ($name, $decl) = @_;

    my $prevdecl = $decls->{fc $name};
    $callback->($decl, $prevdecl) if defined $prevdecl && defined $callback;
    $decls->{fc $name} = $decl;
  };

  for my $decl (@{$scope->{declarations}}) {
    if ($decl->{what} eq $DCL_DS) {
      my $qualified = $decl->{qualified};
      if (!$qualified && defined $decl->{likeds}) {
        my $dschain = findlikeds($decl->{likeds}, @{$scopes});
        $qualified = 1 if grep({ $_->{qualified} } @{$dschain});
      }

      if ($qualified) {
        $adddecl->($decl->{name}, $decl);
      }
      else {
        $adddecl->($decl->{name}, $decl) if ($adddsdecl);
        if (defined $decl->{fields}) {
          $adddecl->($_->{name}, $_) for (@{$decl->{fields}});
        }
      }
    }
    else {
      $adddecl->($decl->{name}, $decl);
    }
  }

  if (defined $scope->{procedures}) {
    for my $procname (keys %{$scope->{procedures}}) {
      my $proc = $scope->{procedures}->{$procname};
      $adddecl->($procname, $proc);
    }
  }

  if (defined $scope->{parameters}) {
    $adddecl->($_->{name}, $_) for (@{$scope->{parameters}});
  }

  return 1;
}

sub findlikeds
{
  my ($ref, @scopes) = @_;

  for my $index (0..$#scopes) {
    my $scope = $scopes[$index];
    for (@{$scope->{declarations}}) {
      next unless ($_->{what} eq $DCL_DS);
      next unless (fc $_->{name} eq fc $ref);

      my $ret = [$_];
      if (defined $_->{likeds}) {
        push(@{$ret}, @{findlikeds($_->{likeds}, @scopes[$index..$#scopes])});
      }

      return $ret;
    }
  }

  return [];
}

package RPG::Linter;

sub print_unix
{
  my $self = shift;
  my ($errors) = @_;

  for my $error (@{$errors}) {
    if ($error->{type} eq $LINT_WARN) {
      printf("%s:%d:%d: ${C_WARN}warning:$C_RESET %s [$C_WARN-W%s$C_RESET]\n",
             $error->{file}, $error->{lineno}, $error->{column}, $error->{msg},
             $error->{what});
      $self->print_unix_code($C_WARN, $error);
    }
    elsif ($error->{type} eq $LINT_NOTE) {
      printf("%s:%d:%d: ${C_NOTE}note:$C_RESET %s\n",
             $error->{file}, $error->{lineno}, $error->{column},
             $error->{msg});
      $self->print_unix_code($C_NOTE, $error);
    }
  }

  return $self;
}

sub print_unix_code
{
  my $self = shift;
  my ($color, $error) = @_;

  my $hltext = $error->{line};

  my $pre = $error->{column} - 1;
  # (.\w*) is used to support highlighting '*ON' and '%subst'
  $hltext =~ s{ ^ (.{$pre}) (.\w*) }{
    my $ref = $2;
    ($1 =~ s/\t/ /gr) . $color . $ref . $C_RESET
  }xsmie;
  printf(" %s", $hltext);
  printf("%s${color}^$C_RESET\n", " " x $error->{column});

  return $self;
}

sub print_json
{
  my $self = shift;
  my ($errors) = @_;

  my @mappederrs = map {
    {
      file => $_->{file},
      line => $_->{lineno},
      column => $_->{column},
      severity => $_->{type},
      message => $_->{msg}
    }
  } @{$errors};

  print JSON::encode_json(\@mappederrs);

  return $self;
}

sub error
{
  my $self = shift;
  my ($what, @data) = @_;

  return unless $data[0]->{file} eq $self->{file};

  my $adderr = sub {
    my ($what, $type, $msg, $data) = @_;
    my $err = {
      what => $what,
      type => $type,
      file => $data->{file},
      line => $data->{line},
      lineno => $data->{lineno},
      column => $data->{column},
      msg => $msg
    };
    push(@{$self->{linterrors}}, $err);

    return $err;
  };

  if ($what eq $RULES_UNDEFINED_REFERENCE) {
    $adderr->($what, $LINT_WARN, sprintf("'%s' undeclared", $data[0]->{token}), $data[0]);
  }
  elsif ($what eq $RULES_GLOBAL) {
    $adderr->($what, $LINT_WARN,
              sprintf("global declaration '%s' is not allowed", $data[0]->{name}),
              $data[0]);
  }
  elsif ($what eq $RULES_QUALIFIED) {
    $adderr->($what, $LINT_WARN,
              sprintf("data structure '%s' needs to be qualified", $data[0]->{name}),
              $data[0]);
  }
  elsif ($what eq $RULES_UPPERCASE_CONSTANT) {
    $adderr->($what, $LINT_WARN,
              sprintf("constant '%s' needs to be all uppercase", $data[0]->{name}),
              $data[0]);
  }
  elsif ($what eq $RULES_SHADOW) {
    my $error = $adderr->($what, $LINT_WARN, sprintf("declaration of '%s' shadows a global declaration", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, $LINT_NOTE, "shadowed declaration is here", $data[1]);
    $note->{linksto} = $error;
  }
  elsif ($what eq $RULES_SUBROUTINE) {
    $adderr->($what, $LINT_WARN, sprintf("subroutine '%s' is not allowed", $data[0]->{name}), $data[0]);
  }
  elsif ($what eq $RULES_UPPERCASE_INDICATOR) {
    $adderr->($what, $LINT_WARN, sprintf("indicator '%s' needs to be all uppercase", $data[0]->{token}), $data[0]);
  }
  elsif ($what eq $RULES_INDICATOR) {
    $adderr->($what, $LINT_WARN, sprintf("indicator '%s' is not allowed", $data[0]->{token}), $data[0]);
  }
  elsif ($what eq $RULES_UNUSED_VARIABLE) {
    $adderr->($what, $LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
  }
  elsif ($what eq $RULES_REDEFINING_SYMBOL) {
    my $error = $adderr->($what, $LINT_WARN, sprintf("redefinition of '%s' as a different kind of symbol", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, $LINT_NOTE, "previous definition is here", $data[1]);
    $note->{linksto} = $error;
  }
  elsif ($what eq $RULES_UNREACHABLE_CODE) {
    $adderr->($what, $LINT_WARN, "code will never be executed", $data[0]);
  }
  elsif ($what eq $RULES_SAME_CASING) {
    my $error = $adderr->($what, $LINT_WARN, sprintf("'%s' is not in the same casing as '%s'", $data[0]->{token} || $data[0]->{likeds}, $data[1]->{name}), $data[0]);
    my $note = $adderr->($what, $LINT_NOTE, "definition is here", $data[1]);
    $note->{linksto} = $error;
  }
  else {
    die "unknown lint message";
  }

  return $self;
}

sub lint
{
  my $self = shift;
  my ($scope) = @_;

  $self->{linterrors} = [];
  $self->{file} = $scope->{file};

  if ($self->{rules}->{$RULES_GLOBAL}) {
    $self->lint_global($scope);
  }

  if ($self->{rules}->{$RULES_SHADOW}) {
    $self->lint_shadow($scope);
  }

  if ($self->{rules}->{$RULES_QUALIFIED}) {
    $self->lint_qualified($scope);
  }

  if ($self->{rules}->{$RULES_UPPERCASE_CONSTANT}) {
    $self->lint_uppercase_constant($scope);
  }

  if ($self->{rules}->{$RULES_UNDEFINED_REFERENCE}) {
    $self->lint_undefined_reference($scope);
  }

  if ($self->{rules}->{$RULES_SUBROUTINE}) {
    $self->lint_subroutine($scope);
  }

  if ($self->{rules}->{$RULES_UPPERCASE_INDICATOR}) {
    $self->lint_uppercase_indicator($scope);
  }

  if ($self->{rules}->{$RULES_INDICATOR}) {
    $self->lint_indicator($scope);
  }

  if ($self->{rules}->{$RULES_UNUSED_VARIABLE}) {
    $self->lint_unused_variable($scope);
  }

  if ($self->{rules}->{$RULES_REDEFINING_SYMBOL}) {
    $self->lint_redefining_symbol($scope);
  }

  if ($self->{rules}->{$RULES_UNREACHABLE_CODE}) {
    $self->lint_unreachable_code($scope);
  }

  if ($self->{rules}->{$RULES_SAME_CASING}) {
    $self->lint_same_casing($scope);
  }

  my @errors = sort main::sorterrors @{$self->{linterrors}};
  $self->{linterrors} = \@errors;
  return $self->{linterrors};
}

sub lint_global
{
  my $self = shift;
  my ($scope) = @_;

  for (@{$scope->{declarations}}) {
    # data structure templates are allowed to be global
    if ($_->{what} eq $DCL_S || $_->{what} eq $DCL_DS && !$_->{template}) {
      $self->error($RULES_GLOBAL, $_);
    }
  }

  return $self;
}

sub lint_shadow
{
  my $self = shift;
  my ($scope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  main::loopscopes($scope, sub {
    my @scopes = @_;

    if (defined $gdecls) {
      $decls = { %{$gdecls} };
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    main::declhash($decls, \@scopes, sub {
      my ($decl, $prevdecl) = @_;
      $self->error($RULES_SHADOW, $decl, $prevdecl);
    });
  });

  return $self;
}

sub lint_qualified
{
  my $self = shift;
  my ($scope) = @_;

  main::loopscopes($scope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    for (@{$scope->{declarations}}) {
      if ($_->{what} eq $DCL_DS) {
        my $qualified = $_->{qualified};
        if (!$qualified && defined $_->{likeds}) {
          my $dschain = main::findlikeds($_->{likeds}, @scopes);
          $qualified = 1 if grep({ $_->{qualified} } @{$dschain});
        }

        $self->error($RULES_QUALIFIED, $_) unless $qualified;
      }
    }
  });

  return $self;
}

sub lint_uppercase_constant
{
  my $self = shift;
  my ($scope) = @_;

  main::loopscopes($scope, sub {
      my ($scope) = @_;

      for (@{$scope->{declarations}}) {
        if ($_->{what} eq $DCL_C) {
          next if uc $_->{name} eq $_->{name};

          $self->error($RULES_UPPERCASE_CONSTANT, $_);
        }
      }
  });

  return $self;
}

sub lint_undefined_reference
{
  my $self = shift;
  my ($scope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  main::loopscopes($scope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    if (defined $gdecls) {
      $decls = { %{$gdecls} };
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    main::declhash($decls, \@scopes);

    # check if a 'likeds' is found
    for (@{$scope->{declarations}}) {
      next unless $_->{what} eq $DCL_DS;
      next unless defined $_->{likeds};
      $self->error($RULES_UNDEFINED_REFERENCE, {
        file => $_->{file},
        line => $_->{line},
        lineno => $_->{lineno},
        column => index($_->{line}, $_->{likeds}) + 1,
        token => $_->{likeds}
      }) unless $decls->{fc $_->{likeds}};
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IDENT) {
        unless (defined $decls->{fc $_->{token}}) {
          $self->error($RULES_UNDEFINED_REFERENCE, $_);
        }
        next;
      }

      if ($_->{what} eq $CALC_SUBF) {
        # check to see if the subfield is part of the data structure
        my $token = $_->{token};
        my $dschain = main::findlikeds($_->{ds}, @scopes);
        my $ds = $dschain->[-1];

        unless (grep { $_->{name} eq $token } @{$ds->{fields}}) {
          $self->error($RULES_UNDEFINED_REFERENCE, $_);
        }
        next;
      }

      if ($_->{what} eq $CALC_EXSR) {
        unless (defined $scope->{subroutines}->{fc $_->{name}}) {
          $self->error($RULES_UNDEFINED_REFERENCE, $_);
        }
      }
    }
  });

  return $self;
}

sub lint_subroutine
{
  my $self = shift;
  my ($scope) = @_;

  main::loopscopes($scope, sub {
    my ($scope) = @_;

    for (keys %{$scope->{subroutines}}) {
      my $sub = $scope->{subroutines}->{$_};
      $self->error($RULES_SUBROUTINE, $sub);
    }
  });

  return $self;
}

sub lint_uppercase_indicator
{
  my $self = shift;
  my ($scope) = @_;

  main::loopscopes($scope, sub {
    my ($scope) = @_;

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IND) {
        next if uc $_->{token} eq $_->{token};

        $self->error($RULES_UPPERCASE_INDICATOR, $_);
      }
    }
  });

  return $self;
}

sub lint_indicator
{
  my $self = shift;
  my ($scope) = @_;

  main::loopscopes($scope, sub {
    my ($scope) = @_;

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IND) {
        # Allow *ON, *OFF, *NULL, *BLANK, *BLANKS, and *OMIT
        next if $_->{token} =~ m{
          \* (?: ON | OFF | NULL | BLANK | BLANKS | OMIT )
        }xsmi;

        $self->error($RULES_INDICATOR, $_);
      }
    }
  });

  return $self;
}

sub lint_unused_variable
{
  my $self = shift;
  my ($scope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  my $checkdecls = sub {
    my ($decls) = @_;

    for (keys %{$decls}) {
      my $decl = $decls->{$_};
      next unless $decl->{what} =~ m{ ^ (?: $DCL_SUBF | $DCL_S ) $ }xsmi;

      # skip 'dcl-subf' for now as we don't have control of qualified, and
      # 'likeds' with qualified.
      next if $decl->{what} eq $DCL_SUBF;

      $self->error($RULES_UNUSED_VARIABLE, $decl);
    }
  };

  main::loopscopes($scope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    if (defined $gdecls) {
      $decls = {};
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    main::declhash($decls, \@scopes);

    my $subrs = { %{$scope->{subroutines}} };

    # check if a 'likeds' is found
    for (@{$scope->{declarations}}) {
      next unless $_->{what} eq $DCL_DS;
      next unless defined $_->{likeds};
      if (defined $decls->{fc $_->{name}}) {
        delete $decls->{fc $_->{name}};
      }
      elsif (defined $gdecls->{fc $_->{name}}) {
        delete $gdecls->{fc $_->{name}};
      }
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_EXSR) {
        if (defined $subrs->{fc $_->{token}}) {
          delete $subrs->{fc $_->{token}};
        }
      }
      if ($_->{what} eq $CALC_IDENT) {
        if (defined $decls->{fc $_->{token}}) {
          delete $decls->{fc $_->{token}};
        }
        elsif (defined $gdecls->{fc $_->{token}}) {
          delete $gdecls->{fc $_->{token}};
        }
        next;
      }
    }

    # not global scope
    if ($decls != $gdecls) {
      $checkdecls->($decls);
    }

    for (keys %{$subrs}) {
      my $subr = $subrs->{$_};
      $self->error($RULES_UNUSED_VARIABLE, $subr);
    }

  });

  $checkdecls->($gdecls);

  return $self;
}

sub lint_redefining_symbol
{
  my $self = shift;
  my ($scope) = @_;


  main::loopscopes($scope, sub {
    my @scopes = @_;
    main::declhash({}, [$scopes[0]], sub {
      my ($decl, $prevdecl) = @_;
      $self->error($RULES_REDEFINING_SYMBOL, $decl, $prevdecl);
    });
  });

  return $self;
}

sub lint_unreachable_code
{
  my $self = shift;
  my ($scope) = @_;

  # here be dragons
  my $checkcalcs;

  my $exsrstmt = sub {
    my ($scope, $calc, $calcs, $preturned) = @_;
    my $subname = $calc->{token};
    my $sub = $scope->{subroutines}->{fc $subname};

    my $subcalcs = [ @{$sub->{calculations}} ];
    while (my $calc = shift(@{$subcalcs})) {
      my $unreached = $checkcalcs->($scope, $calc, $subcalcs, $preturned);
      if (defined $unreached) {
        return $unreached;
      }
    }

    return undef;
  };

  my $branchstmt = sub {
    my ($scope, $calcs, $end, $mid, $exit, $preturned) = @_;
    my $calc = $calcs->[0];
    my $r_mid = '^ (?: ' . join('|', @{$mid}) . ') $';
    my $r_end = '^ ' . $end . ' $';
    my $r_exit = '^ (?: ' . join('|', @{$exit}) . ') $';

    outer: while (my $calc = shift(@{$calcs})) {
      if ($calc->{token} =~ m{ $r_exit }xsmi) {
        while (my $nextcalc = shift(@{$calcs})) {
          if ($calc->{stmt} ne $nextcalc->{stmt}) {
            if ($nextcalc->{token} =~ m{ $r_mid }xsmi) {
              ${$preturned} = 0;
              next outer;
            }
            if ($nextcalc->{token} =~ m{ $r_end }xsmi) {
              while (my $nextcalc = shift(@{$calcs})) {
                if ($calc->{stmt} ne $nextcalc->{stmt}) {
                  unshift(@{$calcs}, $nextcalc);
                  return undef;
                }
              }
              return undef;
            }
            return $nextcalc;
          }
        }
      }

      if ($calc->{token} =~ m{ ^ $r_end $ }xsmi) {
        while (my $nextcalc = shift(@{$calcs})) {
          if ($calc->{stmt} ne $nextcalc->{stmt}) {
            unshift(@{$calcs}, $nextcalc);
            return undef;
          }
        }
        return undef;
      }
      else {
        my $unreached = $checkcalcs->($scope, $calc, $calcs, $preturned);
        if (defined $unreached) {
          if ($unreached->{token} !~ m{ $r_mid | $r_end }xsmi) {
            return $unreached;
          }
        }
      }
    }

    return undef;
  };

  $checkcalcs = sub {
    my ($scope, $calc, $calcs, $preturned) = @_;

    return undef unless defined $calc;

    if ($calc->{what} =~ $CALC_EXSR) {
      my $unreached = $exsrstmt->($scope, $calc, $calcs, $preturned);
      return $unreached if defined $unreached;

      # allow every branch word i.e 'else', 'elseif', and 'endif' when in an
      # 'if' branch.
      if (${$preturned}) {
        while (my $nextcalc = shift(@{$calcs})) {
          if ($calc->{stmt} ne $nextcalc->{stmt}) {
            return $nextcalc;
          }
        }
      }
      return undef;
    }

    return undef if $calc->{what} ne $CALC_OPCODE;

    if ($calc->{token} =~ m{ ^ if $ }xsmi) {
      return $branchstmt->($scope, $calcs, 'endif', ['else', 'elseif'],
                           ['return'], $preturned);
    }

    if ($calc->{token} =~ m{ ^ select $ }xsmi) {
      return $branchstmt->($scope, $calcs, 'endsl', ['when', 'other'],
                           ['return'], $preturned);
    }

    if ($calc->{token} =~ m{ ^ do[wu] $ }xsmi) {
      return $branchstmt->($scope, $calcs, 'enddo', [],
                           ['return', 'iter', 'leave'], $preturned);
    }

    if ($calc->{token} =~ m{ ^ for $ }xsmi) {
      return $branchstmt->($scope, $calcs, 'endfor', [],
                           ['return', 'iter', 'leave'], $preturned);
    }

    if ($calc->{token} =~ m{ ^ monitor $ }xsmi) {
      return $branchstmt->($scope, $calcs, 'endmon', ['on-error'],
                           ['return'], $preturned);
    }

    if ($calc->{token} =~ m{ ^ return $ }xsmi) {
      ${$preturned} = 1;
      while (my $nextcalc = shift(@{$calcs})) {
        if ($calc->{stmt} ne $nextcalc->{stmt}) {
          return $nextcalc;
        }
      }
    }

    return undef;
  };

  main::loopscopes($scope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    my $calcs = [ @{$scope->{calculations}} ];
    while (my $calc = shift(@{$calcs})) {
      my $returned = 0;
      my $unreached = $checkcalcs->($scope, $calc, $calcs, \$returned);
      if (defined $unreached) {
        $self->error($RULES_UNREACHABLE_CODE, $unreached);
        last;
      }
    }
  });

  return $self;
}

sub lint_same_casing
{
  my $self = shift;
  my ($scope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  main::loopscopes($scope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    if (defined $gdecls) {
      $decls = { %{$gdecls} };
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    main::declhash($decls, \@scopes, undef, 1);


    # check if a 'likeds' is found
    for (@{$scope->{declarations}}) {
      next unless $_->{what} eq $DCL_DS;
      next unless defined $_->{likeds};
      my $decl = $decls->{fc $_->{likeds}};
      if (defined $decl && $decl->{name} ne $_->{likeds}) {
        $self->error($RULES_SAME_CASING, $_, $decl);
      }
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IDENT) {
        my $decl = $decls->{fc $_->{token}};
        if (defined $decl && $decl->{name} ne $_->{token}) {
          $self->error($RULES_SAME_CASING, $_, $decl);
        }
      }
    }
  });

  return $self;
}

sub new
{
  my $class = shift;
  my $self = {
    rules => $default_rules
  };
  bless($self, $class);

  return $self;
}

sub setoption
{
  my $self = shift;
  my ($option) = @_;

  my $set = 1;
  if ($option =~ m{ ^ no (.*) }xsmig) {
    $option = $1;
    $set = 0;
  }

  if ($option eq "all") {
    for (keys %{$self->{rules}}) {
      $self->{rules}->{$_} = $set;
    }
  } else {
    unless (grep { $_ eq $option } keys %{$self->{rules}}) {
      die "Unknown warning '$option'";
    }
    $self->{rules}->{$option} = $set;
  }
}

1;
