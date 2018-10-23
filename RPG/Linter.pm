use strict;
use warnings;
use v5.16;
use JSON;
use RPG::Constant qw{ :DCL :CALC :COLOR :LINT :RULES };

use Exporter;

my @rules = (
  main::RULES_GLOBAL,
  main::RULES_INDICATOR,
  main::RULES_PARAMETER_MISMATCH,
  main::RULES_QUALIFIED,
  main::RULES_REDEFINING_SYMBOL,
  main::RULES_SAME_CASING,
  main::RULES_SHADOW,
  main::RULES_SUBROUTINE,
  main::RULES_UNDEFINED_REFERENCE,
  main::RULES_UNREACHABLE_CODE,
  main::RULES_UNUSED_PARAMETER,
  main::RULES_UNUSED_PROCEDURE,
  main::RULES_UNUSED_SUBROUTINE,
  main::RULES_UNUSED_VARIABLE,
  main::RULES_UPPERCASE_CONSTANT,
  main::RULES_UPPERCASE_INDICATOR,
);

# turned on with -Wall
my @rules_all = (
  main::RULES_INDICATOR,
  main::RULES_PARAMETER_MISMATCH,
  main::RULES_QUALIFIED,
  main::RULES_REDEFINING_SYMBOL,
  main::RULES_UNDEFINED_REFERENCE,
  main::RULES_UPPERCASE_CONSTANT,
  main::RULES_UPPERCASE_INDICATOR
);

# turned on with -Wextra
my @rules_extra = (
  main::RULES_SAME_CASING,
  main::RULES_UNUSED_PARAMETER,
  main::RULES_UNUSED_PROCEDURE,
  main::RULES_UNUSED_SUBROUTINE,
  main::RULES_UNUSED_VARIABLE,
);

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

    return 1 if grep({ $_->{qualified} } @{$dschain});
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
    if ($error->{type} eq main::LINT_WARN) {
      printf("%s:%d:%d: %swarning:%s %s [%s-W%s%s]\n",
             $error->{file}, $error->{lineno}, $error->{column},
             main::COLOR_WARN, main::COLOR_RESET, $error->{msg},
             main::COLOR_WARN, $error->{what}, main::COLOR_RESET);
      $self->print_unix_code(main::COLOR_WARN, $error);
    }
    elsif ($error->{type} eq main::LINT_NOTE) {
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

  my $json = JSON->new;
  $json->indent(2);
  $json->canonical(1);
  print $json->encode(\@mappederrs);

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

  if ($what eq main::RULES_GLOBAL) {
    $adderr->($what, main::LINT_WARN,
              sprintf("global declaration '%s' is not allowed", $data[0]->{name}),
              $data[0]);
    return $self;
  }

  if ($what eq main::RULES_INDICATOR) {
    $adderr->($what, main::LINT_WARN, sprintf("indicator '%s' is not allowed", $data[0]->{token}), $data[0]);
    return $self;
  }

  if ($what eq main::RULES_PARAMETER_MISMATCH) {
    my $error = $adderr->($what, main::LINT_WARN, sprintf("conflicting types for '%s'", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, main::LINT_NOTE, sprintf("previous declaration of '%s' was here", $data[1]->{name}), $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq main::RULES_QUALIFIED) {
    $adderr->($what, main::LINT_WARN,
              sprintf("data structure '%s' needs to be qualified", $data[0]->{name}),
              $data[0]);
    return $self;
  }

  if ($what eq main::RULES_REDEFINING_SYMBOL) {
    my $error = $adderr->($what, main::LINT_WARN, sprintf("redefinition of '%s' as a different kind of symbol", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, main::LINT_NOTE, "previous definition is here", $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq main::RULES_SAME_CASING) {
    my $error = $adderr->($what, main::LINT_WARN, sprintf("'%s' is not in the same casing as '%s'", $data[0]->{token} || $data[0]->{likeds}, $data[1]->{name}), $data[0]);
    my $note = $adderr->($what, main::LINT_NOTE, "definition is here", $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq main::RULES_SHADOW) {
    my $error = $adderr->($what, main::LINT_WARN, sprintf("declaration of '%s' shadows a global declaration", $data[0]->{name}), $data[0]);
    my $note = $adderr->($what, main::LINT_NOTE, "shadowed declaration is here", $data[1]);
    $note->{linksto} = $error;
    return $self;
  }

  if ($what eq main::RULES_SUBROUTINE) {
    $adderr->($what, main::LINT_WARN, sprintf("subroutine '%s' is not allowed", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UNDEFINED_REFERENCE) {
    $adderr->($what, main::LINT_WARN, sprintf("'%s' undeclared", $data[0]->{token}), $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UNREACHABLE_CODE) {
    $adderr->($what, main::LINT_WARN, "code will never be executed", $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UNUSED_PARAMETER) {
    $adderr->($what, main::LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UNUSED_PROCEDURE) {
    $adderr->($what, main::LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UNUSED_SUBROUTINE) {
    $adderr->($what, main::LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UNUSED_VARIABLE) {
    $adderr->($what, main::LINT_WARN, sprintf("'%s' defined but not used", $data[0]->{name}), $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UPPERCASE_CONSTANT) {
    $adderr->($what, main::LINT_WARN,
              sprintf("constant '%s' needs to be all uppercase", $data[0]->{name}),
              $data[0]);
    return $self;
  }

  if ($what eq main::RULES_UPPERCASE_INDICATOR) {
    $adderr->($what, main::LINT_WARN, sprintf("indicator '%s' needs to be all uppercase", $data[0]->{token}), $data[0]);
    return $self;
  }

  printf(STDERR "%serror:%s unknown lint type '%s'\n",
         main::COLOR_ERR, main::COLOR_RESET, $what);
  exit(2);
}

sub lint
{
  my $self = shift;
  my ($scope) = @_;

  $self->{linterrors} = [];
  $self->{file} = $scope->{file};

  if ($self->{rules}->{main::RULES_GLOBAL}) {
    $self->lint_global($scope);
  }

  if ($self->{rules}->{main::RULES_INDICATOR}) {
    $self->lint_indicator($scope);
  }

  if ($self->{rules}->{main::RULES_PARAMETER_MISMATCH}) {
    $self->lint_parameter_mismatch($scope);
  }

  if ($self->{rules}->{main::RULES_QUALIFIED}) {
    $self->lint_qualified($scope);
  }

  if ($self->{rules}->{main::RULES_REDEFINING_SYMBOL}) {
    $self->lint_redefining_symbol($scope);
  }

  if ($self->{rules}->{main::RULES_SAME_CASING}) {
    $self->lint_same_casing($scope);
  }

  if ($self->{rules}->{main::RULES_SHADOW}) {
    $self->lint_shadow($scope);
  }

  if ($self->{rules}->{main::RULES_SUBROUTINE}) {
    $self->lint_subroutine($scope);
  }

  if ($self->{rules}->{main::RULES_UNDEFINED_REFERENCE}) {
    $self->lint_undefined_reference($scope);
  }

  if ($self->{rules}->{main::RULES_UNREACHABLE_CODE}) {
    $self->lint_unreachable_code($scope);
  }

  if ($self->{rules}->{main::RULES_UNUSED_PARAMETER}) {
    $self->lint_unused_parameter($scope);
  }

  if ($self->{rules}->{main::RULES_UNUSED_PROCEDURE}) {
    $self->lint_unused_procedure($scope);
  }

  if ($self->{rules}->{main::RULES_UNUSED_SUBROUTINE}) {
    $self->lint_unused_subroutine($scope);
  }

  if ($self->{rules}->{main::RULES_UNUSED_VARIABLE}) {
    $self->lint_unused_variable($scope);
  }

  if ($self->{rules}->{main::RULES_UPPERCASE_CONSTANT}) {
    $self->lint_uppercase_constant($scope);
  }

  if ($self->{rules}->{main::RULES_UPPERCASE_INDICATOR}) {
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
      $self->error(main::RULES_GLOBAL, $_);
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
    }
    else {
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

        $self->error(main::RULES_SHADOW, $decl, $prevdecl);
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

        $self->error(main::RULES_QUALIFIED, $_) unless $qualified;
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

          $self->error(main::RULES_UPPERCASE_CONSTANT, $_);
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
    }
    else {
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
      $self->error(main::RULES_UNDEFINED_REFERENCE, {
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
          $self->error(main::RULES_UNDEFINED_REFERENCE, $_);
        }
        next;
      }

      if ($_->{what} eq main::CALC_SUBF) {
        # check to see if the subfield is part of the data structure
        my $token = $_->{token};
        my $dschain = $findlikeds->($_->{ds}, @scopes);
        my $ds = $dschain->[-1];

        unless (grep { $_->{name} eq $token } @{$ds->{fields}}) {
          $self->error(main::RULES_UNDEFINED_REFERENCE, $_);
        }
        next;
      }

      if ($_->{what} eq main::CALC_EXSR) {
        unless (defined $scope->{subroutines}->{fc $_->{name}}) {
          $self->error(main::RULES_UNDEFINED_REFERENCE, $_);
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
      $self->error(main::RULES_SUBROUTINE, $sub);
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

        $self->error(main::RULES_UPPERCASE_INDICATOR, $_);
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

        $self->error(main::RULES_INDICATOR, $_);
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
      $self->error(main::RULES_UNUSED_PARAMETER, $decl);
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
    $self->error(main::RULES_UNUSED_PROCEDURE, $proc);
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
      $self->error(main::RULES_UNUSED_SUBROUTINE, $subr);
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

      $self->error(main::RULES_UNUSED_VARIABLE, $decl) unless $decl == 0;
    }
  };

  $loopscopes->($gscope, sub {
    my @scopes = @_;
    my ($scope) = @scopes;

    if (defined $gdecls) {
      $decls = {};
    }
    else {
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
      if ($_->{what} eq main::DCL_DS && defined $_->{likeds}) {
        my $likeds = fc $_->{likeds};
        if (defined $decls->{$likeds}) {
          $decls->{$likeds} = 0; # marked deleted
        }
        elsif (defined $gdecls->{fc $_->{name}}) {
          $gdecls->{$likeds} = 0; # marked deleted
        }

        next;
      }

      if ($_->{what} eq main::DCL_S && defined $_->{like}) {

        my $like = fc $_->{like};
        if ($like =~ m{ ^ (.+?) \. (.+?) $ }xsmi) {
          # TODO: Check subf as well as struct
          $like = $1;
        }

        if (defined $decls->{$like}) {
          $decls->{$like} = 0; # marked deleted
        }
        elsif (defined $gdecls->{fc $_->{name}}) {
          $gdecls->{$like} = 0; # marked deleted
        }

        next;
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

    if (defined $scope->{subroutines}) {
      for my $subname (keys %{$scope->{subroutines}}) {
        my $sub = $scope->{subroutines}->{$subname};
        for (@{$sub->{calculations}}) {
          if ($_->{what} eq main::CALC_IDENT) {
            if (defined $decls->{fc $_->{token}}) {
              $decls->{fc $_->{token}} = 0; # marked deleted
            }
            elsif (defined $gdecls->{fc $_->{token}}) {
              $gdecls->{fc $_->{token}} = 0; # marked deleted
            }
          }
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

        $self->error(main::RULES_REDEFINING_SYMBOL, $decl, $prevdecl);
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

      ${$preturned} = 0;
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
        $self->error(main::RULES_UNREACHABLE_CODE, $unreached);
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
    }
    else {
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
        $self->error(main::RULES_SAME_CASING, $_, $decl);
      }
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq main::CALC_IDENT) {
        my $decl = $decls->{fc $_->{token}};
        if (defined $decl && $decl->{name} ne $_->{token}) {
          $self->error(main::RULES_SAME_CASING, $_, $decl);
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
        $self->error(main::RULES_PARAMETER_MISMATCH, $proc, $decl);
        next;
      }
      for my $index (0..scalar(@{$decl->{parameters}}) - 1) {
        my $prparm = $decl->{parameters}->[$index];
        my $procparm = $proc->{parameters}->[$index];
        if (!defined $procparm) {
          $self->error(main::RULES_PARAMETER_MISMATCH, $proc, $decl);
          last;
        }
        if (!$cmptype->($prparm->{type}, $procparm->{type})) {
          $self->error(main::RULES_PARAMETER_MISMATCH, $proc, $decl);
          last;
        }
        if (!$cmpkws->($prparm->{kws}, $procparm->{kws})) {
          $self->error(main::RULES_PARAMETER_MISMATCH, $proc, $decl);
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
    rules => {}
  };
  $self->{rules}->{$_} = 0 for @rules;

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
      $self->{rules}->{$_} = $set for @rules_all;
      return $self;
  }

  if ($rule eq "extra") {
      $self->{rules}->{$_} = $set for @rules_extra;
      return $self;
  }

  # exact match
  if (defined $self->{rules}->{$rule}) {
    $self->{rules}->{$rule} = $set;
    return $self;
  }

  # globbed match
  my @matches = grep { $_ =~ m{ ^ $rule }x } @rules;
  if (@matches == 1) {
    $self->{rules}->{$matches[0]} = $set;
    return $self;
  }
  elsif (@matches > 1) {
    printf(STDERR "%serror:%s warning '%s' is ambigious (%s)\n",
           main::COLOR_ERR, main::COLOR_RESET,
           $orig_rule, join(", ", @matches));
    exit(2);
  }

  printf(STDERR "%serror:%s unknown warning '%s'\n",
         main::COLOR_ERR, main::COLOR_RESET, $orig_rule);
  exit(2);
}

1;
