C
C       $Id: hluvvmpxy.f,v 1.2 1996-02-07 18:11:46 dbrown Exp $
C
      SUBROUTINE HLUVVMPXY (X,Y,U,V,UVM,XB,YB,XE,YE,IST)
C
C This routine stands between Vectors and the user call-back routine
C VVMPXY.  When HLUs are not in use, this version of the routine gets
C loaded, so that VVMPXY is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls VVMPXY.
C
      CALL VVMPXY (X,Y,U,V,UVM,XB,YB,XE,YE,IST)
C
      RETURN
C
      END
