// rpglelint: -Wshadow

dcl-s a int(10);

dcl-proc proc1;
  dcl-pi *n;
    a int(10);
    b int(10);
  end-pi;
end-proc;

dcl-proc proc2;
  dcl-pi *n;
    a int(10);
    b int(10);
  end-pi;

  dcl-s proc1 int(10);
end-proc;
