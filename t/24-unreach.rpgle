// rpglelint: -Wunreachable-code

dcl-proc proc;
  if a > 1;
    return;
  endif;
  a = 1;
end-proc;
