// rpglelint: -Wunused-variable

dcl-ds abc qualified;
  a int(10);
end-ds;

dcl-s b like(abc.a);
dcl-s c int(10);
dcl-s d like(c);

b = 1;
