C
C       $Id: hluvvmpxy.f,v 1.5 2008-07-27 00:17:34 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
