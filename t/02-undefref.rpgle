// rpglelint: -Wundefined-reference
dcl-s var1 int(10);
dcl-s var2 int(20);

var3 = 
      var4
      + var1 + var2;

dcl-proc my_proc;
  var1 = var2;
end-proc;
