// rpglelint: -Wunused-variable -Wunused-procedure -Wunused-parameter

dcl-proc myproc;
  dcl-pi *n pointer;
    myparm int(10);
    myint  int(10);
  end-pi;

  myint = 1;
end-proc;

dcl-proc myproc2;
  dcl-s myint int(10);
  myint = 1;
  myint2 = 1;
end-proc;

dcl-s myint int(10);
dcl-s myint2 int(10);

a = myproc2();
