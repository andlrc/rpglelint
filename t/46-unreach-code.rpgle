// rpglelint: -Wunreachable-code
dcl-proc proc;

  if a > 1;
    exsr subr1;
  endif;

  exsr subr2;
  return 'don''t fail here';
  return 'fail here';

  begsr subr1;
    return 'return in if-statement';
  endsr;

  begsr subr2;
  endsr;
end-proc;
