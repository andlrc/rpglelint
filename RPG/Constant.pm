use strict;
use warnings;

use Exporter;

use constant DCL_PROC => 'dcl-proc';
use constant DCL_PR => 'dcl-pr';
use constant DCL_S => 'dcl-s';
use constant DCL_C => 'dcl-c';
use constant DCL_DS => 'dcl-ds';
use constant DCL_SUBF => 'dcl-subf';
use constant DCL_PARM => 'dcl-parm';

use constant CALC_STR => 'str';
use constant CALC_BIF => 'bif';
use constant CALC_IDENT => 'ident';
use constant CALC_SUBF => 'subf';
use constant CALC_NUM => 'num';
use constant CALC_OPCODE => 'opcode';
use constant CALC_EXSR => 'exsr';
use constant CALC_IND => 'ind';
use constant CALC_OP => 'op';

use constant COLOR_WARN => -t 1 ? "\033[1;35m" : '';
use constant COLOR_NOTE => -t 1 ? "\033[1;36m" : '';
use constant COLOR_RESET => -t 1 ? "\033[0m" : '';

our %EXPORT_TAGS = (
  DCL => [
    'DCL_PROC',
    'DCL_PR',
    'DCL_S',
    'DCL_C',
    'DCL_DS',
    'DCL_SUBF',
    'DCL_PARM'
  ],
  CALC => [
    'CALC_STR',
    'CALC_BIF',
    'CALC_IDENT',
    'CALC_SUBF',
    'CALC_NUM',
    'CALC_OPCODE',
    'CALC_EXSR',
    'CALC_IND',
    'CALC_OP'
  ],
  COLOR => [
    'COLOR_WARN',
    'COLOR_NOTE',
    'COLOR_RESET'
  ]
);

our @EXPORT_OK = qw{
  @EXPORT_TAGS->{DCL}
  @EXPORT_TAGS->{CALC}
  @EXPORT_TAGS->{COLOR}
};
