// rpglelint: -Wunused-variable -Wunused-parameter

dcl-s ptr  pointer;
dcl-s ptr2 pointer;
dcl-s a    int(10) based(ptr);
dcl-ds b   based(ptr2) qualified;
  subf int(10);
end-ds;
dcl-s  c   int(10);

a = 1;
b.subf = 1;

dcl-proc myProc;
  dcl-pi *n;
    ptr  pointer;
    ptr2 pointer;
  end-pi;

  dcl-s  a int(10) based(ptr);
  dcl-ds b based(ptr2) qualified;
    subf int(10);
  end-ds;
  dcl-s  c int(10);

  a = 1;
  b.subf = 1;
end-proc;
