// rpglelint: -Wredefining-symbol

dcl-s a int(10);
dcl-s a varchar(32);
dcl-s b int(10);
dcl-proc proc1;
  dcl-s b varchar(32);
  dcl-s c int(10);
  dcl-s c varchar(32);
end-proc;
