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

sub readstmt
{
  my $self = shift;

  my $fh = $self->{fh};
  my $code = <$fh>;
  unless (defined $code) {
    return 0; # no lines to read
  }

  $self->{startlineno} = $.;

  # FIXME: Support multiline strings with '//' in them
  # remove trailing comments
  if ($code =~ s{ // (.*?) $ }{}xsi) {
    push (@{$self->{comments}}, $1);
    if ($1 =~ m { rpglelint: (.*) }xsmi) {
      my @opts = grep { m/./ } split(/\s+/, $1);
      $self->{parser}->{parseopts}->(\@opts);
    }
  }

  # blank line, slurp until something else
  if ($code =~ m{ ^ \s* $ }xsi) {
    return $self->readstmt();
  }

  # compiler directive
  if ($code =~ m{ ^ \s* / .* $ }xsi) {
    $self->{code} = $code;
    return 1;
  }

  # continuously line
  if ($code !~ m{ ; \s* $ }xsmi) {
    while (my $line = <$fh>) {
      # FIXME: Support multiline strings with '//' in them
      # remove trailing comments
      if ($code =~ s{ // (.*?) $ }{}xsi) {
        push (@{$self->{comments}}, $1);
        if ($1 =~ m { rpglelint: (.*) }xsmi) {
          my @opts = grep { m/./ } split(/\s+/, $1);
          $self->{parser}->{parseopts}->(\@opts);
        }
      }

      $code .= $line;

      last if $line =~ m{ ; \s* $ }xsmi;
    }
    $self->{code} = $code;
    return 1;
  }

  # a one lined statement
  $self->{code} = $code;
  return 1;
}

sub new
{
  my $class = shift;
  my ($parser) = @_;

  my $self = {
    file => $parser->{file},
    fh => $parser->{fh},
    parser => $parser,
    comments => []
  };

  bless($self, $class);

  if ($self->readstmt()) {
    return $self;
  }
  else {
    return undef; # no more lines to read
  }
}

1;
