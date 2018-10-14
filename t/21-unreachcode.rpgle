// rpglelint: -Wunreachable-code

dcl-proc proc1;
  return a;
  a = 1;
end-proc;

dcl-proc proc2;
  if a = 1;
    return a;
  endif;
end-proc;

dcl-proc proc3;
  dow a = 1;
    return a;
  enddo;
end-proc;

dcl-proc proc4;
  select;
    when a = 1;
      return a;
  endsl;
end-proc;

dcl-proc proc5;
  monitor;
    a = 1;
  on-error;
    return a;
  endmon;
end-proc;

dcl-proc proc6;
  if a = 1;
    if a > 1;
      return 1;
    endif;
    return 2;
  endif;
end-proc;

dcl-proc proc7;
  a = 1;
  exsr a;
  a = 2;

  begsr a;
    return 1;
  endsr;
end-proc;

dcl-proc proc8;
  a = 1;
  exsr a;
  a = 3;

  begsr a;
    return 1;
    a = 2;
  endsr;
end-proc;

dcl-proc proc9;
  if a = 1;
    return 1;
    a = 2;
  endif;
end-proc;
