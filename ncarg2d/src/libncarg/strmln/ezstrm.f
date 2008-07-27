C
C       $Id: ezstrm.f,v 1.8 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE EZSTRM(U,V,WORK,IMAX,JMAX)
C
      DIMENSION U(IMAX,JMAX), V(IMAX,JMAX), WORK(1)
C
      CALL STRMLN(U,V,WORK,IMAX,IMAX,JMAX,0,IER)
C
      RETURN
      END
