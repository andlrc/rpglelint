// rpglelint: -Wundefined-reference
dcl-ds my_ds;
  is_global int(10);
end-ds;

dcl-ds my_ds2 qualified;
  is_subf int(10);
end-ds;

dcl-ds my_ds2_like likeds(my_ds2);

is_global = 1;
is_global_err = 1;

my_ds2.is_subf = 1;
my_ds2.is_subf_err = 1;

my_ds2_like.is_subf = 1;
my_ds2_like.is_subf_err = 1;

my_ds_err.is_global = 1;
my_ds_err.is_global_arr = 1;
