use strict;
use warnings;
use v5.16;
use RPG::Constant qw{ :DCL :CALC :COLOR };
use JSON;

use Exporter;

my $LINT_NOTE = "note";
my $LINT_WARN = "warning";

my $RULES_GLOBAL = "global";
my $RULES_INDICATOR = "indicator";
my $RULES_PARAMETER_MISMATCH = "parameter-mismatch";
my $RULES_QUALIFIED = "qualified";
my $RULES_REDEFINING_SYMBOL = "redefining-symbol";
my $RULES_SAME_CASING = "same-casing";
my $RULES_SHADOW = "shadow";
my $RULES_SUBROUTINE = "subroutine";
my $RULES_UNDEFINED_REFERENCE = "undefined-reference";
my $RULES_UNREACHABLE_CODE = "unreachable-code";
my $RULES_UNUSED_PARAMETER = "unused-parameter";
my $RULES_UNUSED_PROCEDURE = "unused-procedure";
my $RULES_UNUSED_SUBROUTINE = "unused-subroutine";
my $RULES_UNUSED_VARIABLE = "unused-variable";
my $RULES_UPPERCASE_CONSTANT = "uppercase-constant";
my $RULES_UPPERCASE_INDICATOR = "uppercase-indicator";

# always append to this list
my $rules_numeric = [
  $RULES_GLOBAL,
  $RULES_INDICATOR,
  $RULES_PARAMETER_MISMATCH,
  $RULES_QUALIFIED,
  $RULES_REDEFINING_SYMBOL,
  $RULES_SAME_CASING,
  $RULES_SHADOW,
  $RULES_SUBROUTINE,
  $RULES_UNDEFINED_REFERENCE,
  $RULES_UNREACHABLE_CODE,
  $RULES_UNUSED_PARAMETER,
  $RULES_UNUSED_PROCEDURE,
  $RULES_UNUSED_SUBROUTINE,
  $RULES_UNUSED_VARIABLE,
  $RULES_UPPERCASE_CONSTANT,
  $RULES_UPPERCASE_INDICATOR,
];

# list of all rules turned off
my $rules_default = {};
$rules_default->{$_} = 0 for (@{$rules_numeric});

# turned on with -Wall
my $rules_all = {
  $RULES_INDICATOR => 0,
  $RULES_PARAMETER_MISMATCH => 0,
  $RULES_QUALIFIED => 0,
  $RULES_REDEFINING_SYMBOL => 0,
  $RULES_UNDEFINED_REFERENCE => 0,
  $RULES_UPPERCASE_CONSTANT => 0,
  $RULES_UPPERCASE_INDICATOR => 0,
};

# turned on with -Wextra
my $rules_extra = {
  $RULES_SAME_CASING => 0,
  $RULES_UNUSED_PARAMETER => 0,
  $RULES_UNUSED_PROCEDURE => 0,
  $RULES_UNUSED_SUBROUTINE => 0,
  $RULES_UNUSED_VARIABLE => 0,
};

# exported
sub rules_default
{
  return { %{ $rules_default } };
}

# exported
sub rules_all
{
  return { %{ $rules_all } };
}

# exported
sub rules_extra
{
  return { %{ $rules_extra } };
}

my $cmptype = sub
{
  my ($a, $b) = @_;

  $a = '' unless defined $a;
  $b = '' unless defined $b;

  return $a =~ s{ \s+ }{}xrg eq $b =~ s{ \s+ }{}xrg;
};

my $cmpkws = sub
{
  my ($akws, $bkws) = @_;

  my @akws = sort { $a cmp $b } map { s{ \s+ }{}xrg } @{$akws};
  my @bkws = sort { $a cmp $b } map { s{ \s+ }{}xrg } @{$bkws};

  for my $index (0..$#akws) {
    my $akw = $akws[$index];
    my $bkw = $bkws[$index];
    return 0 unless defined $bkw;
    return 0 unless $akw eq $bkw;
  }

  # same list
  return 1;
};

my $sorterrors = sub($$)
{
  my ($a, $b) = @_;
  my ($al, $bl) = ($a->{linksto}, $b->{linksto});

  # place linked notes after the warnings
  my $ano = defined $al ? $al->{lineno} + 0.1 : $a->{lineno};
  my $bno = defined $bl ? $bl->{lineno} + 0.1 : $b->{lineno};

  return $ano <=> $bno;
};

my $findlikeds;
$findlikeds = sub
{
  my ($ref, @scopes) = @_;

  for my $index (0..$#scopes) {
    my $scope = $scopes[$index];
    for (@{$scope->{declarations}}) {
      next unless ($_->{what} eq main::DCL_DS);
      next unless (fc $_->{name} eq fc $ref);

      my $ret = [$_];
      if (defined $_->{likeds}) {
        push(@{$ret}, @{$findlikeds->($_->{likeds}, @scopes[$index..$#scopes])});
      }

      return $ret;
    }
  }

  return [];
};

my $isdsqual = sub
{
  my ($decl, $scopes) = @_;

  return 0 unless $decl->{what} eq main::DCL_DS;

  return 1 if $decl->{qualified};

  if (defined $decl->{likeds}) {
    my $dschain = $findlikeds->($decl->{likeds}, @{$scopes});
    return if grep({ $_->{qualified} } @{$dschain});
  }

  return 0;
};

# utility function to loop over each scope,
# the provided subroutine will be called with the scopes.
# i.e the first call will be for the global scope with $scope as the argument,
# the preceding calls with be with '($proc, $scope)' as arguments
my $loopscopes = sub
{
  my ($scope, $sub) = @_;

  $sub->($scope);

  for my $procname (keys %{$scope->{procedures}}) {
    my $proc = $scope->{procedures}->{$procname};
    $sub->($proc, $scope);
  }

  return 1;
};

# returns a hash of all declarations in the current scope,
# calls $same is a previous declaration is already defined
my $declhash = sub
{
  my ($decls, $scopes, %cfg) = @_;
  my ($scope) = @{$scopes};

  my $adddecl = sub {
    my ($name, $decl) = @_;

    # skip
    if ($cfg{add}) {
      return unless $cfg{add}->($decl);
    }

    my $prevdecl = $decls->{fc $name};
    if (defined $prevdecl && defined $cfg{same}) {
      $cfg{same}->($decl, $prevdecl);
    }
    $decls->{fc $name} = $decl;
  };

  for my $decl (@{$scope->{declarations}}) {
    $adddecl->($decl->{name}, $decl);

    # add fields from data structures that are not qualified
    if ($decl->{what} eq main::DCL_DS) {
      if (!$isdsqual->($decl, $scopes)) {
        if (defined $decl->{fields}) {
          $adddecl->($_->{name}, $_) for (@{$decl->{fields}});
        }
      }
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

  return $decls;
};

package RPG::Linter;

sub print_unix
{
  my $self = shift;
  my ($errors) = @_;

  for my $error (@{$errors}) {
    if ($error->{type} eq $LINT_WARN) {
      printf("%s:%d:%d: %swarning:%s %s [%s-W%s%s]\n",
             $error->{file}, $error->{lineno}, $error->{column},
             main::COLOR_WARN, main::COLOR_RESET, $error->{msg},
             main::COLOR_WARN, $error->{what}, main::COLOR_RESET);
      $self->print_unix_code(main::COLOR_WARN, $error);
    }
    elsif ($error->{type} eq $LINT_NOTE) {
      printf("%s:%d:%d: %snote:%s %s\n",
             $error->{file}, $error->{lineno}, $error->{column},
             main::COLOR_NOTE, main::COLOR_RESET, $error->{msg});
      $self->print_unix_code(main::COLOR_NOTE, $error);
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
    ($1 =~ s/\t/ /gr) . $color . $ref . main::COLOR_RESET
  }xsmie;
  printf(" %s", $hltext);
  printf("%s%s^%s\n", " " x $error->{column}, $color, main::COLOR_RESET);

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

  if ($what eq $RULES_GLOBAL) {
    $adderr->($what, $LINT_WARN,
              sprintf("global declaration '%s' is not allowed", $data[0]->{name}),
              $data[0]);
    return $self;
  }

  if ($what eq $RULES_INDICATOR) {
    $adderr->($what, $LINT_WARN, sprintf("indicator '%s' is not allowed", $data[0]->{token}), $data[0]);
    return $self;
  }

  if ($what eq $RULES_PARAMETER_MISMATCH) {
    my $error = $adderr->($what, $LINT_WARN, sprintf("conflicting types for '%s'", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, $LINT_NOTE, sprintf("previous declaration of '%s' was here", $data[1]->{name}), $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq $RULES_QUALIFIED) {
    $adderr->($what, $LINT_WARN,
              sprintf("data structure '%s' needs to be qualified", $data[0]->{name}),
              $data[0]);
    return $self;
  }

  if ($what eq $RULES_REDEFINING_SYMBOL) {
    my $error = $adderr->($what, $LINT_WARN, sprintf("redefinition of '%s' as a different kind of symbol", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, $LINT_NOTE, "previous definition is here", $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq $RULES_SAME_CASING) {
    my $error = $adderr->($what, $LINT_WARN, sprintf("'%s' is not in the same casing as '%s'", $data[0]->{token} || $data[0]->{likeds}, $data[1]->{name}), $data[0]);
    my $note = $adderr->($what, $LINT_NOTE, "definition is here", $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq $RULES_SHADOW) {
    my $error = $adderr->($what, $LINT_WARN, sprintf("declaration of '%s' shadows a global declaration", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, $LINT_NOTE, "shadowed declaration is here", $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq $RULES_SUBROUTINE) {
    $adderr->($what, $LINT_WARN, sprintf("subroutine '%s' is not allowed", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq $RULES_UNDEFINED_REFERENCE) {
    $adderr->($what, $LINT_WARN, sprintf("'%s' undeclared", $data[0]->{token}), $data[0]);
    return $self;
  }

  if ($what eq $RULES_UNREACHABLE_CODE) {
    $adderr->($what, $LINT_WARN, "code will never be executed", $data[0]);
    return $self;
  }

  if ($what eq $RULES_UNUSED_PARAMETER) {
    $adderr->($what, $LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq $RULES_UNUSED_PROCEDURE) {
    $adderr->($what, $LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq $RULES_UNUSED_SUBROUTINE) {
    $adderr->($what, $LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq $RULES_UNUSED_VARIABLE) {
    $adderr->($what, $LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq $RULES_UPPERCASE_CONSTANT) {
    $adderr->($what, $LINT_WARN,
              sprintf("constant '%s' needs to be all uppercase", $data[0]->{name}),
              $data[0]);
    return $self;
  }

  if ($what eq $RULES_UPPERCASE_INDICATOR) {
    $adderr->($what, $LINT_WARN, sprintf("indicator '%s' needs to be all uppercase", $data[0]->{token}), $data[0]);
    return $self;
  }


  die "unknown lint message";
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

  if ($self->{rules}->{$RULES_INDICATOR}) {
    $self->lint_indicator($scope);
  }

  if ($self->{rules}->{$RULES_PARAMETER_MISMATCH}) {
    $self->lint_parameter_mismatch($scope);
  }

  if ($self->{rules}->{$RULES_QUALIFIED}) {
    $self->lint_qualified($scope);
  }

  if ($self->{rules}->{$RULES_REDEFINING_SYMBOL}) {
    $self->lint_redefining_symbol($scope);
  }

  if ($self->{rules}->{$RULES_SAME_CASING}) {
    $self->lint_same_casing($scope);
  }

  if ($self->{rules}->{$RULES_SHADOW}) {
    $self->lint_shadow($scope);
  }

  if ($self->{rules}->{$RULES_SUBROUTINE}) {
    $self->lint_subroutine($scope);
  }

  if ($self->{rules}->{$RULES_UNDEFINED_REFERENCE}) {
    $self->lint_undefined_reference($scope);
  }

  if ($self->{rules}->{$RULES_UNREACHABLE_CODE}) {
    $self->lint_unreachable_code($scope);
  }

  if ($self->{rules}->{$RULES_UNUSED_PARAMETER}) {
    $self->lint_unused_parameter($scope);
  }

  if ($self->{rules}->{$RULES_UNUSED_PROCEDURE}) {
    $self->lint_unused_procedure($scope);
  }

  if ($self->{rules}->{$RULES_UNUSED_SUBROUTINE}) {
    $self->lint_unused_subroutine($scope);
  }

  if ($self->{rules}->{$RULES_UNUSED_VARIABLE}) {
    $self->lint_unused_variable($scope);
  }

  if ($self->{rules}->{$RULES_UPPERCASE_CONSTANT}) {
    $self->lint_uppercase_constant($scope);
  }

  if ($self->{rules}->{$RULES_UPPERCASE_INDICATOR}) {
    $self->lint_uppercase_indicator($scope);
  }

  my @errors = sort $sorterrors @{$self->{linterrors}};
  $self->{linterrors} = \@errors;
  return $self->{linterrors};
}

sub lint_global
{
  my $self = shift;
  my ($scope) = @_;

  for (@{$scope->{declarations}}) {
    # data structure templates are allowed to be global
    if ($_->{what} eq main::DCL_S || $_->{what} eq main::DCL_DS && !$_->{template}) {
      $self->error($RULES_GLOBAL, $_);
    }
  }

  return $self;
}

sub lint_shadow
{
  my $self = shift;
  my ($gscope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  $loopscopes->($gscope, sub {
    my @scopes = @_;

    if (defined $gdecls) {
      $decls = { %{$gdecls} };
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    $declhash->($decls, \@scopes,
      add => sub {
        my ($decl) = @_;

        # don't add unqualified data structures
        if ($decl->{what} eq main::DCL_DS && !$isdsqual->($decl, \@scopes)) {
          return 0;
        }

        my %allowed = (
          main::DCL_DS => 1,
          main::DCL_S => 1,
          main::DCL_SUBF => 1,
          main::DCL_PARM => 1,
          main::DCL_C => 1,
          main::DCL_PR => 1,
          main::DCL_PROC => 1,
        );

        return defined $allowed{$decl->{what}};
      },
      same => sub {
        my ($decl, $prevdecl) = @_;

        return if ($prevdecl->{what} eq main::DCL_PR && $decl->{what} eq main::DCL_PROC);

        $self->error($RULES_SHADOW, $decl, $prevdecl);
      }
    );
  });

  return $self;
}

sub lint_qualified
{
  my $self = shift;
  my ($gscope) = @_;

  $loopscopes->($gscope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    for (@{$scope->{declarations}}) {
      if ($_->{what} eq main::DCL_DS) {
        my $qualified = $_->{qualified};
        if (!$qualified && defined $_->{likeds}) {
          my $dschain = $findlikeds->($_->{likeds}, @scopes);
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
  my ($gscope) = @_;

  $loopscopes->($gscope, sub {
      my ($scope) = @_;

      for (@{$scope->{declarations}}) {
        if ($_->{what} eq main::DCL_C) {
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
  my ($gscope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  $loopscopes->($gscope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    if (defined $gdecls) {
      $decls = { %{$gdecls} };
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    $declhash->($decls, \@scopes,
      add => sub {
        my ($decl) = @_;

        my %allowed = (
          main::DCL_DS => 1,
          main::DCL_S => 1,
          main::DCL_SUBF => 1,
          main::DCL_PARM => 1,
          main::DCL_C => 1,
          main::DCL_PR => 1,
          main::DCL_PROC => 1,
        );

        return defined $allowed{$decl->{what}};
      }
    );

    # check if a 'likeds' is found
    for (@{$scope->{declarations}}) {
      next unless $_->{what} eq main::DCL_DS;
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
      if ($_->{what} eq main::CALC_IDENT) {
        unless (defined $decls->{fc $_->{token}}) {
          $self->error($RULES_UNDEFINED_REFERENCE, $_);
        }
        next;
      }

      if ($_->{what} eq main::CALC_SUBF) {
        # check to see if the subfield is part of the data structure
        my $token = $_->{token};
        my $dschain = $findlikeds->($_->{ds}, @scopes);
        my $ds = $dschain->[-1];

        unless (grep { $_->{name} eq $token } @{$ds->{fields}}) {
          $self->error($RULES_UNDEFINED_REFERENCE, $_);
        }
        next;
      }

      if ($_->{what} eq main::CALC_EXSR) {
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
  my ($gscope) = @_;

  $loopscopes->($gscope, sub {
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
  my ($gscope) = @_;

  $loopscopes->($gscope, sub {
    my ($scope) = @_;

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq main::CALC_IND) {
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
  my ($gscope) = @_;

  $loopscopes->($gscope, sub {
    my ($scope) = @_;

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq main::CALC_IND) {
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

sub lint_unused_parameter
{
  my $self = shift;
  my ($gscope) = @_;

  for my $procname (keys %{$gscope->{procedures}}) {
    my $decls = {};
    my $proc = $gscope->{procedures}->{$procname};

    if (defined $proc->{parameters}) {
      $decls->{fc $_->{name}} = $_ for (@{$proc->{parameters}});
    }


    for (@{$proc->{calculations}}) {
      if ($_->{what} eq main::CALC_IDENT) {
        if (defined $decls->{fc $_->{token}}) {
          $decls->{fc $_->{token}} = 0; # marked deleted
        }
      }
    }

    for (keys %{$decls}) {
      my $decl = $decls->{$_};
      next if $decl == 0;
      $self->error($RULES_UNUSED_PARAMETER, $decl);
    }
  };

  return $self;
}

sub lint_unused_procedure
{
  my $self = shift;
  my ($gscope) = @_;

  my $procs = { %{$gscope->{procedures}} };

  $loopscopes->($gscope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq main::CALC_IDENT) {
        if (defined $procs->{fc $_->{token}}) {
          $procs->{fc $_->{token}} = 0; # marked deleted
        }
      }
    }
  });

  for (keys %{$procs}) {
    my $proc = $procs->{$_};
    next if $proc == 0 || $proc->{exported};
    $self->error($RULES_UNUSED_PROCEDURE, $proc);
  }

  return $self;
}

sub lint_unused_subroutine
{
  my $self = shift;
  my ($gscope) = @_;

  $loopscopes->($gscope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    my $subrs = { %{$scope->{subroutines}} };

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq main::CALC_EXSR) {
        if (defined $subrs->{fc $_->{token}}) {
          $subrs->{fc $_->{token}} = 0; # marked deleted
        }
      }
    }

    for my $subname (keys %{$scope->{subroutines}}) {
      my $sub = $scope->{subroutines}->{$subname};
      for (@{$sub->{calculations}}) {
        if ($_->{what} eq main::CALC_EXSR) {
          if (defined $subrs->{fc $_->{token}}) {
            $subrs->{fc $_->{token}} = 0; # marked deleted
          }
        }
      }
    }

    for (keys %{$subrs}) {
      my $subr = $subrs->{$_};
      next if $subr == 0;
      $self->error($RULES_UNUSED_SUBROUTINE, $subr);
    }
  });

  return $self;
}

sub lint_unused_variable
{
  my $self = shift;
  my ($gscope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  my $checkdecls = sub {
    my ($decls) = @_;

    for (keys %{$decls}) {
      my $decl = $decls->{$_};

      # ignore parameters as they are checked with -Wunused-parameter
      next if $decl == 0 || $decl->{what} eq main::DCL_PARM;

      $self->error($RULES_UNUSED_VARIABLE, $decl) unless $decl == 0;
    }
  };

  $loopscopes->($gscope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    if (defined $gdecls) {
      $decls = {};
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    $declhash->($decls, \@scopes,
      add => sub {
        my ($decl) = @_;

        # don't add unqualified data structures
        if ($decl->{what} eq main::DCL_DS && !$isdsqual->($decl, \@scopes)) {
          return 0;
        }

        # skip 'dcl-subf' for now as we don't have control of qualified, and
        # 'likeds' with qualified.
        my %allowed = (
          main::DCL_DS => 1,
          main::DCL_PARM => 1,
          main::DCL_S => 1
        );

        return defined $allowed{$decl->{what}};
      }
    );

    # check if a 'likeds' is found
    for (@{$scope->{declarations}}) {
      next unless $_->{what} eq main::DCL_DS;
      next unless defined $_->{likeds};
      if (defined $decls->{fc $_->{name}}) {
        $decls->{fc $_->{name}} = 0; # marked deleted
      }
      elsif (defined $gdecls->{fc $_->{name}}) {
        $gdecls->{fc $_->{name}} = 0; # marked deleted
      }
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq main::CALC_IDENT) {
        if (defined $decls->{fc $_->{token}}) {
          $decls->{fc $_->{token}} = 0; # marked deleted
        }
        elsif (defined $gdecls->{fc $_->{token}}) {
          $gdecls->{fc $_->{token}} = 0; # marked deleted
        }
      }
    }

    # not global scope
    if ($decls != $gdecls) {
      $checkdecls->($decls);
    }
  });

  $checkdecls->($gdecls);

  return $self;
}

sub lint_redefining_symbol
{
  my $self = shift;
  my ($gscope) = @_;


  $loopscopes->($gscope, sub {
    my @scopes = @_;
    $declhash->({}, [$scopes[0]],
      add => sub {
        my ($decl) = @_;

        # don't add unqualified data structures
        if ($decl->{what} eq main::DCL_DS && !$isdsqual->($decl, \@scopes)) {
          return 0;
        }

        my %allowed = (
          main::DCL_DS => 1,
          main::DCL_S => 1,
          main::DCL_SUBF => 1,
          main::DCL_PARM => 1,
          main::DCL_C => 1,
          main::DCL_PR => 1,
          main::DCL_PROC => 1
        );

        return defined $allowed{$decl->{what}};
      },
      same => sub {
        my ($decl, $prevdecl) = @_;

        # dcl-proc isn't redefining a dcl-pr
        return if ($prevdecl->{what} eq main::DCL_PR && $decl->{what} eq main::DCL_PROC);

        $self->error($RULES_REDEFINING_SYMBOL, $decl, $prevdecl);
      }
    );
  });

  return $self;
}

sub lint_unreachable_code
{
  my $self = shift;
  my ($gscope) = @_;

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

    if ($calc->{what} eq main::CALC_EXSR) {
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

    return undef if $calc->{what} ne main::CALC_OPCODE;

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

  $loopscopes->($gscope, sub {
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
  my ($gscope) = @_;

  my $gdecls = undef;
  my $decls = undef;

  $loopscopes->($gscope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    if (defined $gdecls) {
      $decls = { %{$gdecls} };
    } else {
      $gdecls = {};
      $decls = $gdecls;
    }

    $declhash->($decls, \@scopes,
      add => sub {
        my ($decl) = @_;

        my %allowed = (
          main::DCL_DS => 1,
          main::DCL_S => 1,
          main::DCL_SUBF => 1,
          main::DCL_PARM => 1,
          main::DCL_C => 1,
          main::DCL_PR => 1,
          main::DCL_PROC => 1
        );

        return defined $allowed{$decl->{what}};
      }
    );

    # check if a 'likeds' is found
    for (@{$scope->{declarations}}) {
      next unless $_->{what} eq main::DCL_DS;
      next unless defined $_->{likeds};
      my $decl = $decls->{fc $_->{likeds}};
      if (defined $decl && $decl->{name} ne $_->{likeds}) {
        $self->error($RULES_SAME_CASING, $_, $decl);
      }
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq main::CALC_IDENT) {
        my $decl = $decls->{fc $_->{token}};
        if (defined $decl && $decl->{name} ne $_->{token}) {
          $self->error($RULES_SAME_CASING, $_, $decl);
        }
      }
    }
  });

  return $self;
}

sub lint_parameter_mismatch
{
  my $self = shift;
  my ($gscope) = @_;

  $loopscopes->($gscope, sub {
    my @scopes = @_;

    my $decls = $declhash->({}, \@scopes,
      add => sub {
        my ($decl) = @_;

        my %allowed = (
          main::DCL_PR => 1
        );

        return defined $allowed{$decl->{what}};
      }
    );

    for (keys %{$decls}) {
      my $decl = $decls->{$_};

      my $proc = $gscope->{procedures}->{fc $decl->{name}};
      next unless defined $proc;

      if (!$cmptype->($proc->{returns}, $decl->{returns})) {
        $self->error($RULES_PARAMETER_MISMATCH, $proc, $decl);
        next;
      }
      for my $index (0..scalar(@{$decl->{parameters}}) - 1) {
        my $prparm = $decl->{parameters}->[$index];
        my $procparm = $proc->{parameters}->[$index];
        if (!defined $procparm) {
          $self->error($RULES_PARAMETER_MISMATCH, $proc, $decl);
          last;
        }
        if (!$cmptype->($prparm->{type}, $procparm->{type})) {
          $self->error($RULES_PARAMETER_MISMATCH, $proc, $decl);
          last;
        }
        if (!$cmpkws->($prparm->{kws}, $procparm->{kws})) {
          $self->error($RULES_PARAMETER_MISMATCH, $proc, $decl);
          last;
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
    rules => { %{$rules_default} }
  };
  bless($self, $class);

  return $self;
}

sub setrule
{
  my $self = shift;
  my ($rule) = @_;
  my $orig_rule = $rule;

  my $set = 1;
  if ($rule =~ m{ ^ no - (.*) }xsmig) {
    $rule = $1;
    $set = 0;
  }

  if ($rule eq "all") {
      $self->{rules}->{$_} = $set for (keys %{$rules_all});
      return $self;
  }

  if ($rule eq "extra") {
      $self->{rules}->{$_} = $set for (keys %{$rules_all});
      return $self;
  }

  if ($rule =~ m{ ^ \d+ $ }xsmi) {
    $rule = $rules_numeric->[$rule + 1];
  }

  if (defined $rule && grep { $_ eq $rule } keys %{$rules_default}) {
    $self->{rules}->{$rule} = $set;
    return $self;
  }

  die "unknown rule '-W$orig_rule'";
}

1;
