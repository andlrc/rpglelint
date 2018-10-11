// rpglelint: -Wqualified
dcl-ds 
  
      not_qual;
  sub1 int(10);
  sub2 int(20);
end-ds;

dcl-ds 
    qual qualified;
  sub1 int(10);
  sub2 int(20);
end-ds;

dcl-ds likey_not_qual likeds(not_qual);
dcl-ds likey_qual likeds(qual);
