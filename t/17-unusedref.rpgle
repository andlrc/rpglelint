// rpglelint: -Wunused-variable
dcl-s a int(10);
dcl-s b int(10);
dcl-ds my_ds;
  subf1 int(10);
end-ds;

dcl-pr q;
end-pr;

dcl-ds my_ds2 likeds(my_ds);
dcl-ds my_ds3 likeds(my_ds) qualified;
dcl-ds my_ds4 likeds(my_ds) qualified;

dcl-ds my_ds_x;
  subf2 int(20);
end-ds;

dcl-ds my_ds_y template;
  subf3 int(20);
end-ds;

dcl-ds my_ds5 likeds(my_ds_x) qualified;

my_ds4.subf = a + subf1 + my_ds3.subf;

my_ds5.subf2 = 1;
