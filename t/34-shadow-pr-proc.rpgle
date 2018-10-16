// rpglelint: -Wshadow

dcl-s myint int(10);

dcl-pr myproc;
end-pr;

dcl-proc myproc;
  dcl-s myint int(10);
end-proc;
