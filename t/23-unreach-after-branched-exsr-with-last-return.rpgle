// rpglelint: -Wunreachable-code
if a > 1;
  exsr nothing;
endif;

dow json_forEach(it);
  return;
  a = 1;
enddo;

begsr nothing;
  return;
endsr;
