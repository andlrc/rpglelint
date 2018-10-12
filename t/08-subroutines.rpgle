// rpglelint: -Wsubroutine
dcl-s glob int(10);

exsr my_sub;

begsr my_sub;
  glob += my_proc();
endsr;

dcl-proc my_proc;
  dcl-pi *n varchar(32);
  end-pi;
  dcl-s ret varchar(32);

  exsr my_sub;
  return ret;

  begsr my_sub;
    exsr my_sub2;
  endsr;

  begsr my_sub2;
    ret = '32';
  endsr;
end-proc;
