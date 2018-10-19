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

use constant COLOR_ERR => -t 1 ? "\033[1;31m" : '';
use constant COLOR_WARN => -t 1 ? "\033[1;35m" : '';
use constant COLOR_NOTE => -t 1 ? "\033[1;36m" : '';
use constant COLOR_RESET => -t 1 ? "\033[0m" : '';

use constant LINT_NOTE => "note";
use constant LINT_WARN => "warning";

use constant RULES_GLOBAL => "global";
use constant RULES_INDICATOR => "indicator";
use constant RULES_PARAMETER_MISMATCH => "parameter-mismatch";
use constant RULES_QUALIFIED => "qualified";
use constant RULES_REDEFINING_SYMBOL => "redefining-symbol";
use constant RULES_SAME_CASING => "same-casing";
use constant RULES_SHADOW => "shadow";
use constant RULES_SUBROUTINE => "subroutine";
use constant RULES_UNDEFINED_REFERENCE => "undefined-reference";
use constant RULES_UNREACHABLE_CODE => "unreachable-code";
use constant RULES_UNUSED_PARAMETER => "unused-parameter";
use constant RULES_UNUSED_PROCEDURE => "unused-procedure";
use constant RULES_UNUSED_SUBROUTINE => "unused-subroutine";
use constant RULES_UNUSED_VARIABLE => "unused-variable";
use constant RULES_UPPERCASE_CONSTANT => "uppercase-constant";
use constant RULES_UPPERCASE_INDICATOR => "uppercase-indicator";

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
    'COLOR_ERR',
    'COLOR_WARN',
    'COLOR_NOTE',
    'COLOR_RESET'
  ],
  LINT => [
    'LINT_NOTE',
    'LINT_WARN'
  ],
  RULES => [
    'RULES_GLOBAL',
    'RULES_INDICATOR',
    'RULES_PARAMETER_MISMATCH',
    'RULES_QUALIFIED',
    'RULES_REDEFINING_SYMBOL',
    'RULES_SAME_CASING',
    'RULES_SHADOW',
    'RULES_SUBROUTINE',
    'RULES_UNDEFINED_REFERENCE',
    'RULES_UNREACHABLE_CODE',
    'RULES_UNUSED_PARAMETER',
    'RULES_UNUSED_PROCEDURE',
    'RULES_UNUSED_SUBROUTINE',
    'RULES_UNUSED_VARIABLE',
    'RULES_UPPERCASE_CONSTANT',
    'RULES_UPPERCASE_INDICATOR'
  ]
);

our @EXPORT_OK = qw{
  @EXPORT_TAGS->{DCL}
  @EXPORT_TAGS->{CALC}
  @EXPORT_TAGS->{COLOR}
  @EXPORT_TAGS->{LINT}
  @EXPORT_TAGS->{RULES}
};
