// rpglelint: -Wqualified
dcl-ds not_qual;
  sub1 int(10);
  sub2 int(20);
end-ds;

dcl-ds qual qualified;
  sub1 int(10);
  sub2 int(20);
end-ds;

dcl-proc my_proc;
  dcl-ds lnot_qual likeds(not_qual);
  dcl-ds llnot_qual likeds(lnot_qual);
  dcl-ds lqual likeds(qual);
  dcl-ds llqual likeds(lqual);

  dcl-ds not_qual2;
    sub1 int(10);
    sub2 int(20);
  end-ds;

  dcl-ds qual2 qualified;
    sub1 int(10);
    sub2 int(20);
  end-ds;

  dcl-ds lnot_qual2 likeds(not_qual2);
  dcl-ds llnot_qual2 likeds(lnot_qual2);
  dcl-ds lqual2 likeds(qual2);
  dcl-ds l_qual2 likeds(lqual2);
end-proc;
