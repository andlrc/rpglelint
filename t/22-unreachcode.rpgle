// rpglelint: -Wunreachable-code

dcl-proc proc1;
  if a > 1;
  endif;

  b = 1;

  return;
end-proc;

dcl-proc proc2;
  dow a > 1;
    iter;
    a = 1;
  enddo;
end-proc;

dcl-proc proc3;
  exsr a;
  a = 1;

  begsr a;
    dow a > 1;
      return 1;
    enddo;
  endsr;
end-proc;

dcl-proc proc4;
  exsr a;
  a = 1;

  begsr a;
    dow a > 1;
      leave;
    enddo;
  endsr;
end-proc;
