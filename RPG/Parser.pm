use strict;
use warnings;
use v5.16;

use Exporter;

our @EXPORT_OK = qw{ parse };

my $DCL_PROC = 'dcl-proc';
my $DCL_PR = 'dcl-pr';
my $DCL_S = 'dcl-s';
my $DCL_C = 'dcl-c';
my $DCL_DS = 'dcl-ds';
my $DCL_SUBF = 'dcl-subf';

my $CALC_STR = 'str';
my $CALC_BIF = 'bif';
my $CALC_IDENT = 'ident';
my $CALC_SUBF = 'subf';
my $CALC_NUM = 'num';
my $CALC_IND = 'ind';
my $CALC_OP = 'op';

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
my $R_IND = '\* (?: ON | OFF | NULL | BLANK | BLANKS | IN[0-0][0-9] | INH[1-9] | INL[1-9] | INLR | INU[1-8] | INRT )';

sub strjoin
{
  my ($str) = @_;

  $str =~ s{ - \n }{}xsmig;

  return $str;
}

sub findfile
{
  my ($file, @path) = @_;

  return $file if -f $file;

  my $f = $file;
  $f =~ s{ / }{.lib/}xsmig;
  $f =~ s{ , }{.file/}xsmig;

  for my $lib (@path) {
    my $slug = "$lib/$f";

    return $slug     if -f $slug;
    return lc($slug) if -f lc($slug);

    return $slug . ".rpgleinc"     if -f $slug . ".rpgleinc";
    return lc($slug . ".rpgleinc") if -f lc($slug . ".rpgleinc");

    return $slug . ".mbr"     if -f $slug . ".mbr";
    return lc($slug . ".mbr") if -f lc($slug . ".mbr");
  }

  # fallback, return input
  return $file;
}

sub getlinenocol
{
  my ($stmt, $ref) = @_;
  my ($line, $lineno, $col) = (undef, $., 0);

  for (split(/(?<=\n)/, $stmt)) {
    if ($col == 0) {
      $line = $_;
      if (m{ (.*?) \b \Q$ref\E \b }xsmi) {
        $col = length($1) + 1;
      }
    } else {
      $lineno--;
    }
  }

  return ($line, $lineno, $col);
}

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
  my $fh = $self->{fh};

  my @params;

  while (<$fh>) {
    # skip blank lines
    next if m{ ^ \s* $ }xsmi;

    # skip comments
    next if m{ ^ \s* // }xsmi;

    last unless m{ (?: dcl-subf \s+ )? ($R_IDENT) \s+ ($R_TYPE) (?: \s+ ($R_KWS) )? }xsmi;

    my @kws = split(/\s+/, defined $3 ? $3 : '');

    my ($line, $lineno, $column) = main::getlinenocol($_, $1);
    push(@params, {
        what => $DCL_SUBF,
        file => $self->{file},
        name => $1,
        type => $2,
        stmt => $_,
        line => $line,
        lineno => $lineno,
        column => $column,
        kws => \@kws
    });
  }

  return \@params;
}

sub adddecl
{
  my $self = shift;
  my ($what, $decl) = @_;
  ($decl->{line}, $decl->{lineno}, $decl->{column}) = main::getlinenocol($self->{stmt}, $decl->{name});
  $decl->{what} = $what;
  $decl->{stmt} = $self->{stmt};
  $decl->{file} = $self->{file};

  push(@{$self->{scope}->{declarations}}, $decl);

  return $decl;
}

sub warn
{
  my $self = shift;
  my ($msg) = @_;

  printf(STDERR "'%s': while parsing the file '%s'", $msg, $self->{file});
  if (defined $.) {
    printf(STDERR "at line %d\n", $.);
  }
  else {
    printf(STDERR "\n");
  }
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

  my $fh;
  if ($self->{file} eq "-") {
    $fh = *STDIN;
  } else {
    if (!open($fh, "<", $self->{file})) {
      $self->warn("$self->{file} $!");
      return undef;
    }
  }
  $self->{fh} = $fh;
  while ($self->{stmt} = <$fh>) {
    # skip blank lines
    next if $self->{stmt} =~ m{ ^ \s* $ }xsmi;

    if ($self->{stmt} =~ m{ ^ \s* / \s* (?: copy | include ) \s+ (.*?) \s* $ }xsmi) {
      my $parser = RPG::Parser::Parser->new;
      $parser->{include} = $parser->{include};

      my $file = main::findfile($1, @{$self->{include}});
      my $s = $parser->parse($file);
      if (defined $s) {
        push(@{$self->{scope}->{declarations}}, @{$s->{declarations}});
      }
      next;
    }

    # skip blank lines
    next if $self->{stmt} =~ m{ ^ \s* $ }xsmi;

    # comments, try to extract liter options, i.e:
    # // rpglelint: -Wundefined-reference -Wnoshadow
    if ($self->{stmt} =~ m{ ^ \s* // }xsmi) {
      if ($self->{stmt} =~ m { rpglelint: (.*) }xsmi) {
        my @opts = grep { m/./ } split(/\s+/, $1);
        $self->{parseopts}->(\@opts);
      }
    }

    # other compiler directive
    next if $self->{stmt} =~ m{ ^ \s* / }xsmi;

    # join continuously lines
    unless ($self->{stmt} =~ m{ ; $ }xsmi) {
      while (my $line = <$fh>) {
        $self->{stmt} .= $line;
        last if $line =~ m{ ; $ }xsmi;
      }
    }

    # FIXME: ctl-opt
    next if $self->{stmt} =~ m{ ctl-opt }xsmi;

    # dcl-proc
    if ($self->{stmt} =~ m{ ^ \s* dcl-proc \s+ ($R_IDENT) ( \s+ export )? }xsmi) {
      my $currentproc = {
        exported => defined $2,
        declarations => [],
        calculations => [],
        subroutines => {}
      };

      $self->{scope}->{procedures}->{$1} = $currentproc;
      $self->setscope($currentproc);
      next;
    }

    if ($self->{stmt} =~ m{ ^ \s* end-proc \s* ; }xsmi) {
      $self->popscope();
      next;
    }

    # dcl-pr
    if ($self->{stmt} =~ m{ ^ \s* dcl-pr \s+ ($R_IDENT) (?: \s+ ($R_TYPE) )? }xsmi) {
      $self->adddecl($DCL_PR, {
        name => $1,
        returns => defined $2 ? $2 : '',
        parameters => $self->subf()
      });
      $self->warn("expected 'end-pr'") unless m{ end-pr }xsmi;
      next;
    }

    # dcl-pi
    if ($self->{stmt} =~ m{ ^ \s* dcl-pi \s+ ($R_IDENT | \*N) (?: \s+ ($R_TYPE) )? }xsmi) {
      $self->{scope}->{returns} = $2 if defined $2;
      $self->{scope}->{parameters} = $self->subf();
      $self->warn("expected 'end-pi'") unless m{ end-pi }xsmi;
      next;
    }

    # dcl-s
    if ($self->{stmt} =~ m{ ^ \s* dcl-s \s+ ($R_IDENT) \s+ ($R_TYPE) (?: \s+ ($R_KWS) )? }xsmi) {
      my @kws = split(/\s+/, defined $3 ? $3 : '');
      $self->adddecl($DCL_S, {
        name => $1,
        type => defined $2 ? $2 : '',
        kws => \@kws
      });

      next;
    }

    # dcl-c
    if ($self->{stmt} =~ m{ ^ \s* dcl-c \s+ ($R_IDENT) (?: \s+ const \s* \( (.*?) \) | (.*?) ) ; }xsmi) {
      $self->adddecl($DCL_C, {
        name => $1,
        value => ($2 or $3)
      });

      next;
    }

    # dcl-ds
    if ($self->{stmt} =~ m{ ^ \s* dcl-ds \s+ ($R_IDENT) (?: \s+ ($R_KWS) )? }xsmi) {
      my @kws = split(/\s+/, defined $2 ? $2 : '');

      my $decl = $self->adddecl($DCL_DS, {
        name => $1,
        likeds => undef,
        qualified => 0,
        template => 0,
        kws => \@kws
      });

      for (@kws) {
        if (m{ likeds \( (.*?) \) }xsmi) {
          $decl->{likeds} = $1;
        } elsif (m{ qualified }xsmi) {
          $decl->{qualified} = 1;
        } elsif (m{ template }xsmi) {
          $decl->{template} = 1;
        }
      }

      unless (defined $decl->{likeds}) {
        $decl->{fields} = $self->subf();
        $self->warn("expected 'end-ds'") unless m{ end-ds }xsmi;
      }

      next;
    }

    # subroutines
    if ($self->{stmt} =~ m{ ^ \s* begsr \s+ ($R_IDENT) }xsmi) {
      my $sub = {
        file => $self->{file},
        name => $1,
        stmt => $self->{stmt},
        calculations => [],
      };
      ($sub->{line}, $sub->{lineno}, $sub->{column}) = main::getlinenocol($self->{stmt}, $sub->{name});

      $self->{scope}->{subroutines}->{$1} = $sub;
      $self->setscope($sub);
      next;
    }

    if ($self->{stmt} =~ m{ ^ \s* endsr \s* ; }xsmi) {
      $self->popscope();
      next;
    }

    my $startlineno = $. - (() = $self->{stmt} =~ m{ \n }xsmig) + 1;
    while (my $kw = $self->{stmt} =~ m{ ( $R_STR | $R_BIF | $R_SUBF | $R_IDENT | $R_NUM | $R_IND | $R_OP ) }xsmigp) {
      my @prelines = split(/\n/, ${^PREMATCH});
      my $calc = {
        file => $self->{file},
        lineno => $startlineno,
        column => 1,
        line => '',
        token => $1,
        stmt => $self->{stmt}
      };
      if (@prelines) {
        $calc->{lineno} += @prelines - 1;
        $calc->{line} = pop(@prelines);
        $calc->{column} += length($calc->{line});
      }
      $calc->{line} .= $calc->{token};
      $calc->{line} .= ${^POSTMATCH} =~ s{ \n .* }{}xsmir . $/;

      if ($calc->{token} =~ m{ $R_STR }xsmi) {
        $calc->{what} = $CALC_STR;
 
        # join continuously character literals
        $calc->{token} =~ s{ ' (.*?) ' }{"'" . main::strjoin($1) . "'"}xsmieg;
      }
      elsif ($calc->{token} =~ m{ ^ $R_BIF $ }xsmi) {
        $calc->{what} = $CALC_BIF;
      }
      elsif ($calc->{token} =~ m{ ^ $R_SUBF $ }xsmi) {
        $calc->{what} = $CALC_SUBF;
        $calc->{column}++;
        $calc->{token} = substr($calc->{token}, 1);
        $calc->{ds} = $self->{scope}->{calculations}[-1]->{token};
      }
      elsif ($calc->{token} =~ m{ ^ $R_IND $ }xsmi) {
        $calc->{what} = $CALC_IND;
      }
      elsif ($calc->{token} =~ m{ ^ $R_NUM $ }xsmi) {
        $calc->{what} = $CALC_NUM;
      }
      elsif ($calc->{token} =~ m{ ^ $R_OP $ }xsmi) {
        $calc->{what} = $CALC_OP;
      }
      elsif ($calc->{token} =~ m{ ^ $R_IDENT $ }xsmi) {
        $calc->{what} = $CALC_IDENT;
      }

      push(@{$self->{scope}->{calculations}}, $calc);
    }
  }

  if ($self->{file} ne "-") {
    close($fh);
  }

  return $self->{rootscope};
}

1;
