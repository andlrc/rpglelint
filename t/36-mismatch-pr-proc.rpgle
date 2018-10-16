// rpglelint: -Wparameter-mismatch

dcl-pr proc1 int(10);
  a int(10);
  b int( 10 );
end-pr;

dcl-pr proc2 int(10);
  a int(10);
  b varchar(32);
end-pr;

dcl-pr proc3 int(10);
  a int(10);
  b int(10) options(*nopass);
end-pr;

dcl-pr proc4 int(10);
  a int(10) value;
  b int(10) value options(*nopass);
end-pr;

dcl-proc proc1;
  dcl-pi *n int( 10 );
    b int(10);
    a int(10);
  end-pi;
end-proc;

dcl-proc proc2;
  dcl-pi *n int(10);
    x int(10);
    y int(10);
  end-pi;
end-proc;

dcl-proc proc3;
  dcl-pi *n int(10);
    x int(10) options(*nopass);
    y int(10);
  end-pi;
end-proc;

dcl-proc proc4;
  dcl-pi *n int(10);
    x int(10) value;
    b int(10) options(*nopass) value;
  end-pi;
end-proc;
