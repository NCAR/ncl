C
C $Id: ctcell.f,v 1.5 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CTCELL (Z,MX,NX,NY,M,I0,J0)
C
C CTCELL COMPUTES LINES OF CONSTANT Z (CONTOUR LINES) IN ONE
C CELL OF THE ARRAY Z FOR THE SRFACE PACKAGE.
C Z,MX,NX,NY ARE THE SAME AS IN SRFACE.
C M          BY THE TIME CTCELL IS FIRST CALLED, M CONTAINS
C            THE TWO-SPACE PLOTTER LOCATION OF EACH Z POINT.
C            U(Z(I,J))=M(1,I,J).  V(Z(I,J))=M(2,I,J)
C I0,J0      THE CELL Z(I0,J0) TO Z(I0+1,J0+1) IS THE ONE TO
C            BE CONTOURED.
C
      DIMENSION       Z(MX,NY)   ,M(2,NX,NY)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      DATA IDUB/0/
      R(HO,HU) = (HO-CV)/(HO-HU)
      I1 = I0
      I1P1 = I1+1
      J1 = J0
      J1P1 = J1+1
      H1 = Z(I1,J1)
      H2 = Z(I1,J1P1)
      H3 = Z(I1P1,J1P1)
      H4 = Z(I1P1,J1)
      IF (IOFFP .NE. 1) GO TO  10
      IF (H1.EQ.SPVAL .OR. H2.EQ.SPVAL .OR. H3.EQ.SPVAL .OR.
     1    H4.EQ.SPVAL) RETURN
   10 IF (MIN(H1,H2,H3,H4) .GT. CL(NCL)) RETURN
      DO 110 K=1,NCL
C
C FOR EACH CONTOUR LEVEL, DESIDE WHICH OF THE 16 BASIC SIT-
C UATIONS EXISTS, THEN INTERPOLATE IN TWO-SPACE TO FIND THE
C END POINTS OF THE CONTOUR LINE SEGMENT WITHIN THIS CELL.
C
         CV = CL(K)
         K1 = (INT(SIGN(1.,H1-CV))+1)/2
         K2 = (INT(SIGN(1.,H2-CV))+1)/2
         K3 = (INT(SIGN(1.,H3-CV))+1)/2
         K4 = (INT(SIGN(1.,H4-CV))+1)/2
         JUMP = 1+K1+K2*2+K3*4+K4*8
         GO TO (120, 30, 50, 60, 70, 20, 80, 90, 90, 80,
     1           40, 70, 60, 50, 30,110),JUMP
   20    IDUB = 1
   30    RA = R(H1,H2)
         MUA = REAL(M(1,I1,J1))+RA*REAL(M(1,I1,J1P1)-M(1,I1,J1))
         MVA = REAL(M(2,I1,J1))+RA*REAL(M(2,I1,J1P1)-M(2,I1,J1))
         RB = R(H1,H4)
         MUB = REAL(M(1,I1,J1))+RB*REAL(M(1,I1P1,J1)-M(1,I1,J1))
         MVB = REAL(M(2,I1,J1))+RB*REAL(M(2,I1P1,J1)-M(2,I1,J1))
         GO TO 100
   40    IDUB = -1
   50    RA = R(H2,H1)
         MUA = REAL(M(1,I1,J1P1))+RA*REAL(M(1,I1,J1)-M(1,I1,J1P1))
         MVA = REAL(M(2,I1,J1P1))+RA*REAL(M(2,I1,J1)-M(2,I1,J1P1))
         RB = R(H2,H3)
         MUB = REAL(M(1,I1,J1P1))+RB*REAL(M(1,I1P1,J1P1)-M(1,I1,J1P1))
         MVB = REAL(M(2,I1,J1P1))+RB*REAL(M(2,I1P1,J1P1)-M(2,I1,J1P1))
         GO TO 100
   60    RA = R(H2,H3)
         MUA = REAL(M(1,I1,J1P1))+RA*REAL(M(1,I1P1,J1P1)-M(1,I1,J1P1))
         MVA = REAL(M(2,I1,J1P1))+RA*REAL(M(2,I1P1,J1P1)-M(2,I1,J1P1))
         RB = R(H1,H4)
         MUB = REAL(M(1,I1,J1))+RB*REAL(M(1,I1P1,J1)-M(1,I1,J1))
         MVB = REAL(M(2,I1,J1))+RB*REAL(M(2,I1P1,J1)-M(2,I1,J1))
         GO TO 100
   70    RA = R(H3,H2)
         MUA = REAL(M(1,I1P1,J1P1))+
     1         RA*REAL(M(1,I1,J1P1)-M(1,I1P1,J1P1))
         MVA = REAL(M(2,I1P1,J1P1))+
     1         RA*REAL(M(2,I1,J1P1)-M(2,I1P1,J1P1))
         RB = R(H3,H4)
         MUB = REAL(M(1,I1P1,J1P1))+
     1         RB*REAL(M(1,I1P1,J1)-M(1,I1P1,J1P1))
         MVB = REAL(M(2,I1P1,J1P1))+
     1         RB*REAL(M(2,I1P1,J1)-M(2,I1P1,J1P1))
         IDUB = 0
         GO TO 100
   80    RA = R(H2,H1)
         MUA = REAL(M(1,I1,J1P1))+RA*REAL(M(1,I1,J1)-M(1,I1,J1P1))
         MVA = REAL(M(2,I1,J1P1))+RA*REAL(M(2,I1,J1)-M(2,I1,J1P1))
         RB = R(H3,H4)
         MUB = REAL(M(1,I1P1,J1P1))+
     1         RB*REAL(M(1,I1P1,J1)-M(1,I1P1,J1P1))
         MVB = REAL(M(2,I1P1,J1P1))+
     1         RB*REAL(M(2,I1P1,J1)-M(2,I1P1,J1P1))
         GO TO 100
   90    RA = R(H4,H1)
         MUA = REAL(M(1,I1P1,J1))+RA*REAL(M(1,I1,J1)-M(1,I1P1,J1))
         MVA = REAL(M(2,I1P1,J1))+RA*REAL(M(2,I1,J1)-M(2,I1P1,J1))
         RB = R(H4,H3)
         MUB = REAL(M(1,I1P1,J1))+RB*REAL(M(1,I1P1,J1P1)-M(1,I1P1,J1))
         MVB = REAL(M(2,I1P1,J1))+RB*REAL(M(2,I1P1,J1P1)-M(2,I1P1,J1))
         IDUB = 0
  100    CALL DRAWS (MUA,MVA,MUB,MVB,1,0)
         IF (IDUB)  90,110, 70
  110 CONTINUE
  120 RETURN
      END
