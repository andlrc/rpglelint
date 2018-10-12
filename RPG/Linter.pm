use strict;
use warnings;
use v5.16;

use Data::Dumper;
use Exporter;

my $DCL_DS = 'dcl-ds';
my $DCL_S = 'dcl-s';
my $DCL_C = 'dcl-c';

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
my $RULES_UCCONST = "ucconst";
my $RULES_UNDEFREF = "undefref";
my $RULES_SUBROUTINE = "subroutine";
my $RULES_UCINDICATOR = "ucindicator";
my $RULES_INDICATOR = "indicator";

my $default_rules = {
  global => 0,
  shadow => 0,
  qualified => 0,
  ucconst => 0,
  undefref => 0,
  subroutine => 0,
  ucindicator => 0,
  indicator => 0
};

# utility function to loop over each scope,
# the provided subroutine will be called with the scopes.
# i.e the first call will be for the global scope with $scope as the argument,
# the preceeding calls with be with $proc, $scope as arguments
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

  for (@{$scope->{declarations}}) {
    my $decl = $decls->{$_->{name}};

    $callback->($_, $decl) if defined $decl && defined $callback;

    if ($_->{what} eq $DCL_DS) {
      my $qualified = 0;
      if ($_->{qualified}) {
        $qualified = 1;
      }
      elsif (defined $_->{likeds}) {
        my $dschain = findlikeds($_->{likeds}, @{$scopes});
          $qualified = 1 if grep({ $_->{qualified} } @{$dschain});
      }

      if ($qualified) {
        $decls->{$_->{name}} = $_;
      }
      elsif (defined $_->{fields}) {
        my $ds = $_;
        for (@{$_->{fields}}) {
          my $decl = $decls->{$_->{name}};
          $callback->($_, $decl) if defined $decl && defined $callback;
          $decls->{$_->{name}} = $_;
        }
      }
    }
    else {
      $decls->{$_->{name}} = $_;
    }
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

package RPG::Linter::Linter;

sub print_unix
{
  my $self = shift;
  my ($what, $type, $line, $msg) = @_;

  if ($type eq $LINT_WARN) {
    printf(STDERR "%s:%d:%d: ${C_WARN}warning:$C_RESET %s [$C_WARN-W%s$C_RESET]\n",
           $line->{file}, $line->{lineno}, $line->{column}, $msg, $what);
    $self->print_unix_code($type, $line);
  } elsif ($type eq $LINT_NOTE) {
    printf(STDERR "%s:%d:%d: ${C_WARN}note:$C_RESET %s\n",
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

  if ($self->{rules}->{$RULES_UCCONST}) {
    $self->lint_ucconst($scope);
  }

  if ($self->{rules}->{$RULES_UNDEFREF}) {
    $self->lint_undefref($scope);
  }

  if ($self->{rules}->{$RULES_SUBROUTINE}) {
    $self->lint_subroutine($scope);
  }

  if ($self->{rules}->{$RULES_UCINDICATOR}) {
    $self->lint_ucindicator($scope);
  }

  if ($self->{rules}->{$RULES_INDICATOR}) {
    $self->lint_indicator($scope);
  }

  for my $error (sort {
      $a->{data}[0]->{lineno} <=> $b->{data}[0]->{lineno};
    } @{$self->{linterrors}}) {
    my ($what, @data) = ($error->{what}, @{$error->{data}});

    if ($what eq $RULES_UNDEFREF) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("'%s' undeclared", $data[0]->{token}));
    }
    elsif ($what eq $RULES_GLOBAL) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("global declaration '%s' is not allowed", $data[0]->{name}));
    }
    elsif ($what eq $RULES_QUALIFIED) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("data structure '%s' needs to be qualified", $data[0]->{name}));
    }
    elsif ($what eq $RULES_UCCONST) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("constant '%s' needs to be all uppercase", $data[0]->{name}));
    }
    elsif ($what eq $RULES_SHADOW) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("declaration of '%s' shadows a global declaration", $data[0]->{name}));
      $self->print_unix($what, $LINT_NOTE, $data[1], "shadowed declaration is here");
    }
    elsif ($what eq $RULES_SUBROUTINE) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("subroutine '%s' is not allowed", $data[0]->{name}));
    }
    elsif ($what eq $RULES_UCINDICATOR) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("indicator '%s' needs to be all uppercase", $data[0]->{token}));
    }
    elsif ($what eq $RULES_INDICATOR) {
      $self->print_unix($what, $LINT_WARN, $data[0], sprintf("indicator '%s' is not allowed", $data[0]->{token}));
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
        if (defined $_->{likeds}) {
          my $dschain = main::findlikeds($_->{likeds}, @scopes);
          if (!grep({ $_->{qualified} } @{$dschain})) {
            $self->error($RULES_QUALIFIED, $_);
          }
        } elsif (!$_->{qualified}) {
          $self->error($RULES_QUALIFIED, $_);
        }
      }
    }
  });

  return $self;
}

sub lint_ucconst
{
  my $self = shift;
  my ($scope) = @_;

  main::loopscopes($scope, sub {
      my ($scope) = @_;

      for (@{$scope->{declarations}}) {
        if ($_->{what} eq $DCL_C) {
          next if uc $_->{name} eq $_->{name};

          $self->error($RULES_UCCONST, $_);
        }
      }
  });

  return $self;
}

sub lint_undefref
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

    # check if a likeds is found
    for (@{$scope->{declarations}}) {
      next unless $_->{what} eq $DCL_DS;
      next unless defined $_->{likeds};
      $self->error($RULES_UNDEFREF, {
        file => $_->{file},
        line => $_->{line},
        lineno => $_->{lineno},
        column => index($_->{line}, $_->{likeds}) + 1,
        token => $_->{likeds}
      }) unless $decls->{$_->{likeds}};
    }

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IDENT) {
        # check if ident is defined
        $self->error($RULES_UNDEFREF, $_) unless defined $decls->{$_->{token}};
      }
      elsif ($_->{what} eq $CALC_SUBF) {
        # check to see if the subf is part of the ds
        my $token = $_->{token};
        my $dschain = main::findlikeds($_->{ds}, @scopes);
        my $ds = $dschain->[-1];

        unless (grep { $_->{name} eq $token } @{$ds->{fields}}) {
          $self->error($RULES_UNDEFREF, $_);
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

sub lint_ucindicator
{
  my $self = shift;
  my ($scope) = @_;

  main::loopscopes($scope, sub {
    my ($scope) = @_;

    for (@{$scope->{calculations}}) {
      if ($_->{what} eq $CALC_IND) {
        next if uc $_->{token} eq $_->{token};

        $self->error($RULES_UCINDICATOR, $_);
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
