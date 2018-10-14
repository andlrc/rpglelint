// rpglelint: -Wunreachable-code

dcl-proc proc1;
  if forceLock;
    return 0;
  endif;

  if a > 1;
    b = 1;
    return 1;
  else;
    c = 1;
    return 2;
  endif;
end-proc;


