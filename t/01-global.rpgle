// rpglelint: -Wglobal
dcl-s global1 varchar(32);
dcl-ds global2;
  globalsub1 int(10);
  globalsub2 int(20);
end-ds;

dcl-proc a;
  dcl-s local1 varchar(32);
  dcl-ds local2;
    localsub1 int(10);
    localsub2 int(20);
  end-ds;
end-proc;
