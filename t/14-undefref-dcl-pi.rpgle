// rpglelint: -Wundefined-reference

dcl-s a int(10);
dcl-proc myProc;
  dcl-pi *n pointer;
    pDest pointer;
    pSrc  pointer;
  end-pi;

  dcl-s b int(10);

  dcl-pr doIt;
    *n pointer;
    *n pointer;
  end-pr;

  doIt(pDest : pSrc);

  c = doIt(a : b);
end-proc;
