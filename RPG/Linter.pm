use strict;
use warnings;
use v5.16;

use Data::Dumper;
use Exporter;

my $DCL_DS = 'dcl-ds';
my $DCL_S = 'dcl-s';
my $DCL_C = 'dcl-c';
my $DCL_SUBF = 'dcl-subf';

my $CALC_IDENT = 'ident';
my $CALC_SUBF = 'subf';
my $CALC_IND = 'ind';

my $C_WARN = -t 2 ? "\033[1;35m" : '';
my $C_NOTE = -t 2 ? "\033[1;36m" : '';
my $C_RESET = -t 2 ? "\033[0m" : '';

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
  'redefining-symbol' => 0
};

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
  my ($decls, $scopes, $callback) = @_;
  my ($scope) = @{$scopes};

  my $adddecl = sub {
    my ($name, $decl) = @_;

    my $prevdecl = $decls->{$name};
    $callback->($decl, $prevdecl) if defined $prevdecl && defined $callback;
    $decls->{$name} = $decl;
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
      elsif (defined $decl->{fields}) {
        $adddecl->($_->{name}, $_) for (@{$decl->{fields}});
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
  my ($what, $type, $line, $msg) = @_;

  if ($type eq $LINT_WARN) {
    printf(STDERR "%s:%d:%d: ${C_WARN}warning:$C_RESET %s [$C_WARN-W%s$C_RESET]\n",
           $line->{file}, $line->{lineno}, $line->{column}, $msg, $what);
    $self->print_unix_code($type, $line);
  } elsif ($type eq $LINT_NOTE) {
    printf(STDERR "%s:%d:%d: ${C_NOTE}note:$C_RESET %s\n",
           $line->{file}, $line->{lineno}, $line->{column}, $msg);
    $self->print_unix_code($type, $line);
  }

  return $self;
}

sub print_unix_code
{
  my $self = shift;
  my ($type, $line, $msg) = @_;
  my $color = '';

  if ($type eq $LINT_WARN) {
    $color = $C_WARN;
  } elsif ($type eq $LINT_NOTE) {
    $color = $C_NOTE;
  }

  my $hltext = $line->{line};

  my $pre = $line->{column} - 1;
  # (.\w*) is used to support highlighting '*ON' and '%subst'
  $hltext =~ s{ ^ (.{$pre}) (.\w*) }{$1${color}$2$C_RESET}xsmi;
  printf(STDERR " %s", $hltext);
  printf(STDERR "%s${color}^$C_RESET\n", " " x $line->{column});

  return $self;
}

sub error
{
  my $self = shift;
  my ($what, @data) = @_;

  push(@{$self->{linterrors}}, {
    what => $what,
    data => \@data
  }) if $data[0]->{file} eq $self->{file};

  return $self;
}

sub lint
{
  my $self = shift;
  my ($scope) = @_;

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

  for my $error (sort {
      $a->{data}[0]->{lineno} <=> $b->{data}[0]->{lineno};
    } @{$self->{linterrors}}) {
    my ($what, @data) = ($error->{what}, @{$error->{data}});

    if ($what eq $RULES_UNDEFINED_REFERENCE) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("'%s' undeclared", $data[0]->{token}));
    }
    elsif ($what eq $RULES_GLOBAL) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("global declaration '%s' is not allowed", $data[0]->{name}));
    }
    elsif ($what eq $RULES_QUALIFIED) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("data structure '%s' needs to be qualified", $data[0]->{name}));
    }
    elsif ($what eq $RULES_UPPERCASE_CONSTANT) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("constant '%s' needs to be all uppercase", $data[0]->{name}));
    }
    elsif ($what eq $RULES_SHADOW) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("declaration of '%s' shadows a global declaration", $data[0]->{name}));
      $self->print_unix($what, $LINT_NOTE, $data[1], "shadowed declaration is here");
    }
    elsif ($what eq $RULES_SUBROUTINE) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("subroutine '%s' is not allowed", $data[0]->{name}));
    }
    elsif ($what eq $RULES_UPPERCASE_INDICATOR) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("indicator '%s' needs to be all uppercase", $data[0]->{token}));
    }
    elsif ($what eq $RULES_INDICATOR) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("indicator '%s' is not allowed", $data[0]->{token}));
    }
    elsif ($what eq $RULES_UNUSED_VARIABLE) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("'%s' defined but not used", $data[0]->{name}));
    }
    elsif ($what eq $RULES_REDEFINING_SYMBOL) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("redefinition of '%s' as a different kind of symbol", $data[0]->{name}));
      $self->print_unix($what, $LINT_NOTE, $data[1], "previous definition is here");
    } else {
      die "unknown lint message";
    }
  }

  return $self;
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
      }) unless $decls->{$_->{likeds}};
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IDENT) {
        # check if identifier is defined
        $self->error($RULES_UNDEFINED_REFERENCE, $_) unless defined $decls->{$_->{token}};
      }
      elsif ($_->{what} eq $CALC_SUBF) {
        # check to see if the subfield is part of the data structure
        my $token = $_->{token};
        my $dschain = main::findlikeds($_->{ds}, @scopes);
        my $ds = $dschain->[-1];

        unless (grep { $_->{name} eq $token } @{$ds->{fields}}) {
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
        # Allow *ON, *OFF, *NULL, *BLANK, and *BLANKS
        next if $_->{token} =~ m{ \* (?: ON | OFF | NULL | BLANK | BLANKS ) }xsmi;

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
      delete $decls->{$_->{name}};
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IDENT) {
        delete $decls->{$_->{token}};
      }
    }

    for (keys %{$decls}) {
      my $decl = $decls->{$_};

      next if $decl->{what} ne $DCL_SUBF and $decl->{what} ne $DCL_S;

      # skip 'dcl-subf' for now as we don't have control of qualified, and 'likeds'
      # with qualified.
      next if $decl->{what} eq $DCL_SUBF;

      $self->error($RULES_UNUSED_VARIABLE, $decl);
    }
  });

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

sub new
{
  my $class = shift;
  my $self = {
    rules => $default_rules,
    linterrors => []
  };
  bless($self, $class);

  return $self;
}

1;
