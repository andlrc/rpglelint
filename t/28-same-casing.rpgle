// rpglelint: -Wsame-casing

dcl-ds MyDS;
  SUBF1 int(10);
end-ds;

dcl-s abc int(10);

dcl-ds myds2 likeds(myds);
dcl-ds myds2 likeds(MyDS);

subf1 = 1;
abc = 1;
Abc = 1;
SUBF1 = 1;
