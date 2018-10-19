use strict;
use warnings;
use v5.16;
use RPG::Statement;
use RPG::Constant qw{ :DCL :CALC };

use File::Basename;

use Exporter;

my $R_IDENT = '(?! \d ) \w+';
my $R_TYPE = '  (?: date | ind | object | pointer | time | timestamp | value )'
           . '| (?: bindec | char | class | date | float | graph | int | like '
           . '    | likeds | likefile | likerec | object | options | packed'
           . '    | pointer | time | timestamp | ucs2 | uns | varchar'
           . '    | vargraph | varucs2 | zoned ) \s*\(\s* .*? \s*\)';
my $R_KWS = '[^;]+';
my $R_STR = '\'(\'\'|[^\'])*?\'';
my $R_NUM = '\d+';
my $R_SUBF = '\. '. $R_IDENT;
my $R_OP = '(?:<> | >= | <= | > | < | = | \+= | -= | \+ | - | or | and | not)';
my $R_BIF = '% ' . $R_IDENT;
my $R_OPCODE = '(?: \b select \b | \b when \b | \b other \b | \b endsl \b'
             . '  | \b if \b | \b elseif \b | \b else \b | \b endif \b'
             . '  | \b to \b | \b or \b | \b and \b | \b not \b'
             . '  | \b do[uw] \b | \b iter \b | \b leave \b | \b enddo \b'
             . '  | \b for \b | \b endfor \b'
             . '  | \b begsr \b | \b leavesr \b | \b endsr \b'
             . '  | \b monitor \b | \b on-error \b | \b endmon \b'
             . '  | \b return \b)';

my $R_IND = '\* (?: ON | OFF | NULL | BLANK | BLANKS | OMIT'
          . '     | IN[0-0][0-9] | INH[1-9]  | INL[1-9] | INLR | INU[1-8] | INRT )';

my $strjoin = sub
{
  my ($str) = @_;

  $str =~ s{ - \n }{}xsmig;

  return $str;
};

my $findfile = sub
{
  my ($file, @path) = @_;

  for my $prefix ("", @path) {
    my $f = $file;
    $f =~ s{ / }{.lib/}xsmig;
    $f =~ s{ , }{.file/}xsmig;

    for ($file, $f) {
      my $slug = $_;
      $slug = "$prefix/$slug" if $prefix;

      return $slug     if -f $slug;
      return lc($slug) if -f lc($slug);

      return $slug . ".rpgleinc"     if -f $slug . ".rpgleinc";
      return lc($slug . ".rpgleinc") if -f lc($slug . ".rpgleinc");

      return $slug . ".mbr"     if -f $slug . ".mbr";
      return lc($slug . ".mbr") if -f lc($slug . ".mbr");
    }
  }

  return undef;
};

package RPG::Parser;

sub new
{
  my $class = shift;
  my $self = {
    include => []
  };
  bless($self, $class);

  return $self;
}

sub setscope
{
  my $self = shift;
  my ($scope) = @_;

  $self->{prevscopes} = [] unless (defined $self->{prevscopes});

  push(@{$self->{prevscopes}}, $self->{scope}) if $self->{scope};
  $self->{scope} = $scope;

  return $self;
}

sub popscope
{
  my $self = shift;

  $self->{scope} = pop(@{$self->{prevscopes}});

  return $self;
}

sub subf
{
  my $self = shift;
  my ($what) = @_;

  my @params;

  while (my $stmt = $self->getstmt()) {

    # skip other compiler directive
    next if $stmt->{code} =~ m{ ^ \s* / }xsmi;

    last unless $stmt->{code} =~ m{
      (?: dcl-subf \s+ )? ($R_IDENT) \s+ ($R_TYPE) (?: \s+ ($R_KWS) )?
    }xsmi;

    my @kws = split(/\s+/, defined $3 ? $3 : '');

    $stmt->calckw($1);

    push(@params, {
        what => $what,
        file => $stmt->{file},
        stmt => $stmt,
        name => $1,
        type => $2,
        line => $stmt->{line},
        lineno => $stmt->{lineno},
        column => $stmt->{column},
        kws => \@kws
    });
  }

  return \@params;
}

sub adddecl
{
  my $self = shift;
  my ($what, $decl) = @_;
  my $stmt = $self->{stmt};

  $stmt->calckw($decl->{name});

  $decl->{what} = $what;
  $decl->{stmt} = $stmt;
  $decl->{file} = $stmt->{file};
  $decl->{line} = $stmt->{line};
  $decl->{lineno} = $stmt->{lineno};
  $decl->{column} = $stmt->{column};

  push(@{$self->{scope}->{declarations}}, $decl);

  return $decl;
}

sub warn
{
  my $self = shift;
  my ($msg) = @_;
  my ($package, $filename, $line) = caller;

  if (defined $self->{stmt}) {
    printf(STDERR "Parse Warning:\n"
                . ">> Message:      %s\n"
                . ">> Current File: %s at line %d\n"
                . ">> Source File:  %s at line %d\n",
                $msg,
                $self->{file}, $self->{stmt}->{startlineno},
                $filename, $line);
  }
  else {
    printf(STDERR "Parse Warning:\n"
                . ">> Message:      %s\n"
                . ">> Current File: %s\n"
                . ">> Source File:  %s at line %d\n",
                $msg,
                $self->{file},
                $filename, $line);
  }
}

sub getstmt
{
  my $self = shift;
  my $fh = $self->{fh};

  my $stmt = RPG::Statement->new($self);
  $self->{stmt} = $stmt;
  return $stmt;
}

sub serialize
{
  my $self = shift;
  my ($scope) = @_;

  my $dump = {};
  $dump->{what} = $scope->{what} if defined $scope->{what};
  $dump->{name} = $scope->{name} if defined $scope->{name};
  $dump->{file} = $scope->{file} if defined $scope->{file};
  $dump->{token} = $scope->{token} if defined $scope->{token};
  $dump->{type} = $scope->{type} if defined $scope->{type};
  $dump->{kws} = $scope->{kws} if defined $scope->{kws} && @{$scope->{kws}};
  if (defined $scope->{stmt}->{comments} && @{$scope->{stmt}->{comments}}) {
    $dump->{comments} = $scope->{stmt}->{comments};
  }

  for (@{$scope->{declarations}}) {
    push(@{$dump->{declarations}}, $self->serialize($_));
  }

  for (@{$scope->{calculations}}) {
    push(@{$dump->{calculations}}, $self->serialize($_));
  }

  for my $name (keys %{$scope->{procedures}}) {
    push(@{$dump->{procedures}}, $self->serialize($scope->{procedures}->{$name}));
  }

  for my $name (keys %{$scope->{subroutines}}) {
    push(@{$dump->{subroutines}}, $self->serialize($scope->{subroutines}->{$name}));
  }

  for (@{$scope->{parameters}}) {
    push(@{$dump->{parameters}}, $self->serialize($_));
  }

  for (@{$scope->{fields}}) {
    push(@{$dump->{fields}}, $self->serialize($_));
  }

  return $dump;
}

sub parse
{
  my $self = shift;
  $self->{file} = shift;

  $self->{rootscope} = {
    file => $self->{file},
    declarations => [],
    calculations => [],
    procedures => {},
    subroutines => {}
  };
  $self->setscope($self->{rootscope});

  if ($self->{file} eq "-") {
    $self->{fh} = *STDIN;
  }
  else {
    if (!open($self->{fh}, "<", $self->{file})) {
      $self->warn("$self->{file} $!");
      return undef;
    }
  }

  while (my $stmt = $self->getstmt()) {

    if ($stmt->{code} =~ m{
        ^ \s* / \s* (?: copy | include ) \s+ (.*?) \s* $
      }xsmi) {
      my $file = $findfile->($1, main::dirname($self->{file}), @{$self->{include}});
      if (defined $file) {
        my $parser = RPG::Parser->new;
        $parser->{include} = $self->{include};
        my $s = $parser->parse($file);
        if (defined $s) {
          push(@{$self->{scope}->{declarations}}, @{$s->{declarations}});
        }
      }
      else {
        $self->warn("Failed to include file '$1'");
      }
      next;
    }

    # skip other compiler directive
    next if $stmt->{code} =~ m{ ^ \s* / }xsmi;

    # FIXME: ctl-opt
    next if $stmt->{code} =~ m{ ctl-opt }xsmi;

    # dcl-proc
    if ($stmt->{code} =~ m{
        ^ \s* dcl-proc \s+ ($R_IDENT) ( \s+ export )?
      }xsmi) {
      $stmt->calckw($1);
      my $proc = {
        name => $1,
        file => $stmt->{file},
        what => main::DCL_PROC,
        stmt => $stmt,
        line => $stmt->{line},
        lineno => $stmt->{lineno},
        column => $stmt->{column},
        exported => defined $2,
        declarations => [],
        calculations => [],
        subroutines => {}
      };

      $self->{scope}->{procedures}->{fc $1} = $proc;
      $self->setscope($proc);
      next;
    }

    if ($stmt->{code} =~ m{ ^ \s* end-proc \s* ; }xsmi) {
      $self->popscope();
      next;
    }

    # dcl-pr
    if ($stmt->{code} =~ m{
        ^ \s* dcl-pr \s+ ($R_IDENT) (?: \s+ ($R_TYPE) )?
      }xsmi) {
      my $decl = $self->adddecl(main::DCL_PR, {
        name => $1,
        returns => defined $2 ? $2 : ''
      });
      $decl->{parameters} = $self->subf(main::DCL_PARM);

      unless ($self->{stmt}->{code} =~ m{ end-pr }xsmi) {
        $self->warn("expected 'end-pr'");
      }

      next;
    }

    # dcl-pi
    if ($stmt->{code} =~ m{
        ^ \s* dcl-pi \s+ ($R_IDENT | \*N) (?: \s+ ($R_TYPE) )?
      }xsmi) {

      $self->{scope}->{returns} = $2 if defined $2;
      $self->{scope}->{parameters} = $self->subf(main::DCL_PARM);

      unless ($self->{stmt}->{code} =~ m{ end-pi }xsmi) {
        $self->warn("expected 'end-pi'");
      }
      next;
    }

    # dcl-s
    if ($stmt->{code} =~ m{
        ^ \s* dcl-s \s+ ($R_IDENT) \s+ ($R_TYPE) (?: \s+ ($R_KWS) )?
      }xsmi) {

      my @kws = split(/\s+/, defined $3 ? $3 : '');
      $self->adddecl(main::DCL_S, {
        name => $1,
        type => defined $2 ? $2 : '',
        kws => \@kws
      });

      next;
    }

    # dcl-c
    if ($stmt->{code} =~ m{
        ^ \s* dcl-c \s+ ($R_IDENT) (?: \s+ const \s* \( (.*?) \) | (.*?) ) ;
      }xsmi) {
      $self->adddecl(main::DCL_C, {
        name => $1,
        value => ($2 or $3)
      });

      next;
    }

    # dcl-ds
    if ($stmt->{code} =~ m{
        ^ \s* dcl-ds \s+ ($R_IDENT) (?: \s+ ($R_KWS) )?
      }xsmi) {
      my @kws = split(/\s+/, defined $2 ? $2 : '');

      my $decl = $self->adddecl(main::DCL_DS, {
        name => $1,
        likeds => undef,
        qualified => 0,
        template => 0,
        kws => \@kws
      });

      for (@kws) {
        if (m{ likeds \( (.*?) \) }xsmi) {
          $decl->{likeds} = $1;
        }
        elsif (m{ qualified }xsmi) {
          $decl->{qualified} = 1;
        }
        elsif (m{ template }xsmi) {
          $decl->{template} = 1;
        }
      }

      next if (defined $decl->{likeds});
      next if grep { m { extname }xsmi } @kws;

      # sub-fields
      $decl->{fields} = $self->subf(main::DCL_SUBF);
      unless ($self->{stmt}->{code} =~ m{ end-ds }xsmi) {
        $self->warn("expected 'end-ds'");
      }

      next;
    }

    # subroutines
    if ($stmt->{code} =~ m{ ^ \s* begsr \s+ ($R_IDENT) }xsmi) {
      $stmt->calckw($1);
      my $sub = {
        file => $self->{file},
        name => $1,
        stmt => $stmt,
        line => $stmt->{line},
        lineno => $stmt->{lineno},
        column => $stmt->{column},
        calculations => [],
      };

      $self->{scope}->{subroutines}->{fc $1} = $sub;
      $self->setscope($sub);
      next;
    }

    if ($stmt->{code} =~ m{ ^ \s* endsr \s* ; }xsmi) {
      $self->popscope();
      next;
    }

    if ($stmt->{code} =~ m{ ^ \s* exsr \s+ ($R_IDENT) \s* ; }xsmi) {
      $stmt->calckw($1);
      push(@{$self->{scope}->{calculations}}, {
        what => main::CALC_EXSR,
        file => $self->{file},
        name => $1,
        token => $1, # TODO: Should be removed?
        stmt => $stmt,
        line => $stmt->{line},
        lineno => $stmt->{lineno},
        column => $stmt->{column}
      });
      next;
    }

    while (my $kw = $stmt->{code} =~ m{
        ( $R_STR | $R_BIF | $R_SUBF | $R_OPCODE | $R_IDENT | $R_NUM | $R_IND | $R_OP )
      }xsmigp) {

      my @prelines = split(/\n/, ${^PREMATCH});
      my $calc = {
        file => $self->{file},
        lineno => $stmt->{startlineno},
        stmt => $stmt,
        column => 1,
        line => '',
        token => $1
      };
      if (@prelines) {
        $calc->{lineno} += @prelines - 1;
        $calc->{line} = pop(@prelines);
        $calc->{column} += length($calc->{line});
      }
      $calc->{line} .= $calc->{token};
      $calc->{line} .= ${^POSTMATCH} =~ s{ \n .* }{}xsmir . $/;

      if ($calc->{token} =~ m{ ^ $R_STR $ }xsmi) {
        $calc->{what} = main::CALC_STR;
        # join continuously character literals
        $calc->{token} =~ s{ ' (.*?) ' }{"'" . $strjoin->($1) . "'"}xsmieg;
      }
      elsif ($calc->{token} =~ m{ ^ $R_BIF $ }xsmi) {
        $calc->{what} = main::CALC_BIF;
      }
      elsif ($calc->{token} =~ m{ ^ $R_SUBF $ }xsmi) {
        $calc->{what} = main::CALC_SUBF;
        $calc->{column}++;
        $calc->{token} = substr($calc->{token}, 1);
        $calc->{ds} = $self->{scope}->{calculations}[-1]->{token};
      }
      elsif ($calc->{token} =~ m{ ^ $R_OPCODE $ }xsmi) {
        $calc->{what} = main::CALC_OPCODE;
      }
      elsif ($calc->{token} =~ m{ ^ $R_IND $ }xsmi) {
        $calc->{what} = main::CALC_IND;
      }
      elsif ($calc->{token} =~ m{ ^ $R_NUM $ }xsmi) {
        $calc->{what} = main::CALC_NUM;
      }
      elsif ($calc->{token} =~ m{ ^ $R_OP $ }xsmi) {
        $calc->{what} = main::CALC_OP;
      }
      elsif ($calc->{token} =~ m{ ^ $R_IDENT $ }xsmi) {
        $calc->{what} = main::CALC_IDENT;
      }

      push(@{$self->{scope}->{calculations}}, $calc);
    }
  }

  if ($self->{file} ne "-") {
    close($self->{fh});
  }

  return $self->{rootscope};
}

1;
