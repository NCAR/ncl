C
C $Id: seradp.f,v 1.5 2008-07-27 00:17:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SERADP (FB,FA2,FA4,FC1,FC3,LAM)
C
C     COMPUTES INTEGRAL FUNCTION OF TRANSFORMED LONG. FOR FOURIER
C     CONSTANTS A2, A4, B, C1, AND C3.
C     LAM IS INTEGRAL VALUE OF TRANSFORMED LONG.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      COMMON /NORMDP/ Q,T,U,W,ES,P22,SA,CA,XJ
      DATA DG1 /0.01745329252D0/
      DATA ONE,TWO /1.0D0,2.0D0/
      LAM=LAM*DG1
      SD=SIN(LAM)
      SDSQ=SD*SD
      S=P22*SA*COS(LAM)*SQRT((ONE+T*SDSQ)/((ONE+W*SDSQ)
     .  *(ONE+Q*SDSQ)))
      H=SQRT((ONE+Q*SDSQ)/(ONE+W*SDSQ))*(((ONE+W*SDSQ)/
     .   ((ONE+Q*SDSQ)**TWO))-P22*CA)
      SQ=SQRT(XJ*XJ+S*S)
      FB=(H*XJ-S*S)/SQ
      FA2=FB*COS(TWO*LAM)
      FA4=FB*COS(4.0D0*LAM)
      FC=S*(H+XJ)/SQ
      FC1=FC*COS(LAM)
      FC3=FC*COS(3.0D0*LAM)
      RETURN
      END
