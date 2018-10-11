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

my $R_NAME = '(?! \d ) \w+';
my $R_TYPE = '  (?: date | ind | object | pointer | time | timestamp | value )'
           . '| (?: bindec | char | class | date | float | graph | int | like '
           . '    | likeds | likefile | likerec | object | options | packed'
           . '    | pointer | time | timestamp | ucs2 | uns | varchar'
           . '    | vargraph | varucs2 | zoned ) \s*\(\s* .*? \s*\)';
my $R_KWS = '[^;]+';

sub findfile
{
  my ($file, @path) = @_;

  return $file if -f $file;

  my $f = $file;
  $f =~ s{ / }{.lib/}xsmig;
  $f =~ s{ , }{.file/}xsmig;

  for my $lib (@path) {
    return $lib ."/". $f . ".rpgleinc" if -f $lib ."/". $f . ".rpgleinc";
    return $lib ."/". $f . ".mbr" if -f $lib ."/". $f . ".mbr";
  }

  for my $lib (@path) {
    return lc($lib ."/". $f . ".rpgleinc") if -f lc($lib ."/". $f . ".rpgleinc");
    return lc($lib ."/". $f . ".mbr") if -f lc($lib ."/". $f . ".mbr");
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

package RPG::Parser::Parser;

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

    last unless m{ (?: dcl-subf \s+ )? ($R_NAME) \s+ ($R_TYPE) (?: \s+ ($R_KWS) )? }xsmi;

    my @kws = split(/\s+/, defined $3 ? $3 : '');

    my ($line, $lineno, $column) = main::getlinenocol($_, $1);
    push(@params, {
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

  printf(STDERR "%s while parsing the file '%s' at line %d\n", $msg, $self->{file}, $.);
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
    # // rpglelint: -Wundefref -Wnoshadow
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
    if ($self->{stmt} =~ m{ dcl-proc \s+ ($R_NAME) ( \s+ export )? }xsmi) {
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

    if ($self->{stmt} =~ m{ end-proc }xsmi) {
      $self->popscope();
      next;
    }

    # dcl-pr
    if ($self->{stmt} =~ m{ dcl-pr \s+ ($R_NAME) (?: \s+ ($R_TYPE) )? }xsmi) {
      $self->adddecl($DCL_PR, {
        name => $1,
        returns => defined $2 ? $2 : '',
        parameters => $self->subf()
      });
      $self->warn("expected 'end-pr'") unless m{ end-pr }xsmi;
      next;
    }

    # dcl-pi
    if ($self->{stmt} =~ m{ dcl-pi \s+ ($R_NAME | \*N) (?: \s+ ($R_TYPE) )? }xsmi) {
      $self->{scope}->{returns} = $2 if defined $2;
      $self->{scope}->{parameters} = $self->subf();
      $self->warn("expected 'end-pi'") unless m{ end-pi }xsmi;
      next;
    }

    # dcl-s
    if ($self->{stmt} =~ m{ dcl-s \s+ ($R_NAME) \s+ ($R_TYPE) (?: \s+ ($R_KWS) )? }xsmi) {
      my @kws = split(/\s+/, defined $3 ? $3 : '');
      $self->adddecl($DCL_S, {
        name => $1,
        type => defined $2 ? $2 : '',
        kws => \@kws
      });

      next;
    }

    # dcl-c
    if ($self->{stmt} =~ m{ dcl-c \s+ ($R_NAME) (?: \s+ const \s* \( (.*?) \) | (.*?) ) ; }xsmi) {
      $self->adddecl($DCL_C, {
        name => $1,
        value => ($2 or $3)
      });

      next;
    }

    # dcl-ds
    if ($self->{stmt} =~ m{ dcl-ds \s+ ($R_NAME) (?: \s+ ($R_KWS) )? }xsmi) {
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
    if ($self->{stmt} =~ m{ begsr \s+ ($R_NAME) }xsmi) {
      my $subroutine = {
        calculations => []
      };

      $self->{scope}->{subroutines}->{$1} = $subroutine;
      $self->setscope($subroutine);
      next;
    }

    if ($self->{stmt} =~ m{ endsr }xsmi) {
      $self->popscope();
      next;
    }

    push(@{$self->{scope}->{calculations}}, $self->{stmt});
  }

  if ($self->{file} ne "-") {
    close($fh);
  }

  return $self->{rootscope};
}

1;
