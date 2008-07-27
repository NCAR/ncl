C
C	$Id: intzs.f,v 1.4 2008-07-27 00:17:22 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE INTZS (XX,YY,ZZ,LIN3,ITOP)
C
C FORCE STORAGE OF X, Y, AND Z INTO COMMON BLOCK
C
      COMMON /PWRZ2S/ X, Y, Z
      DATA IDUMX,IDUMY,IDUMZ /0, 0, 0/
      X = XX
      Y = YY
      Z = ZZ
      CALL INITZS (IDUMX,IDUMY,IDUMZ,LIN3,ITOP,1)
      RETURN
      END
