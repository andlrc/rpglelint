// rpglelint: -Wundefined-reference

exsr undefRef;
exsr shouldFail;

begsr undefRef;
  return 'should work!';
endsr;
