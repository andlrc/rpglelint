// rpglelint: -Wunused-variable
dcl-ds times_t qualified;
  remaining int(10);
  total     int(10);
end-ds;

dcl-ds stageTimes   likeds(times_t);
dcl-ds caseTimes    likeds(times_t);

stageTimes.remaining = 1;
