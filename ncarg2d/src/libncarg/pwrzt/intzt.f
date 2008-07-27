C
C	$Id: intzt.f,v 1.4 2008-07-27 00:17:23 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE INTZT (XX,YY,ZZ,LIN3,ITOP)
C
C FORCE STORAGE OF X, Y, AND Z INTO COMMON BLOCK
C
      COMMON /PWRZ2T/ X, Y, Z
      DATA IDUMX,IDUMY,IDUMZ /0, 0, 0/
      X = XX
      Y = YY
      Z = ZZ
      CALL INITZT (IDUMX,IDUMY,IDUMZ,LIN3,ITOP,1)
      RETURN
      END
