use strict;
use warnings;
use v5.16;

use Exporter;

package RPG::Statement;

sub calckw
{
  my $self = shift;
  my ($ref) = @_;

  $self->{line} = undef;
  $self->{lineno} = $self->{startlineno};
  $self->{column} = 0;

  for (split(/(?<=\n)/, $self->{code})) {
    if ($self->{column} == 0) {
      $self->{line} = $_;
      if (m{ (.*?) \b \Q$ref\E \b }xsmi) {
        $self->{column} = length($1) + 1;
      }
      else {
        $self->{lineno}++;
      }
    }
  }
}

sub new
{
  my $class = shift;
  my ($parser) = @_;

  my $fh = $parser->{fh};

  my $code = <$fh>;

  return undef unless defined $code;

  my $self = {
    file => $parser->{file},
    startlineno => $.
  };

  bless($self, $class);

  # FIXME: Support multiline strings with '//' in them
  # remove trailing comments
  if ($code =~ s{ // (.*?) $ }{}xsi) {
    if ($1 =~ m { rpglelint: (.*) }xsmi) {
      my @opts = grep { m/./ } split(/\s+/, $1);
      $parser->{parseopts}->(\@opts);
    }
  }

  if ($code =~ m{ ^ \s* $ }xsi) {
    # blank line
    $self->{code} = $code;
  }
  elsif ($code =~ m{ ^ \s* / .* $ }xsi) {
    # compiler directive
    $self->{code} = $code;
  }
  elsif ($code !~ m{ ; \s* $ }xsmi) {
    # continuously line
    while (my $line = <$fh>) {
      # FIXME: Support multiline strings with '//' in them
      # remove trailing comments
      if ($code =~ s{ // (.*?) $ }{}xsi) {
        if ($1 =~ m { rpglelint: (.*) }xsmi) {
          my @opts = grep { m/./ } split(/\s+/, $1);
          $parser->{parseopts}->(\@opts);
        }
      }

      $code .= $line;

      last if $line =~ m{ ; \s* $ }xsmi;
    }
    $self->{code} = $code;
  }
  else {
    $self->{code} = $code;
  }

  return $self;
}

1;
