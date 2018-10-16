// rpglelint: -Wunused-variable

exsr shouldWork;

begsr shouldWork;
  return 'should work';
endsr;

begsr shouldFail;
  return 'should fail';
endsr;
