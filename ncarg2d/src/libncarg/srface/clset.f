C
C $Id: clset.f,v 1.6 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CLSET (Z,MX,NX,NY,CHI,CLO,CINC,NLA,NLM,CL,NCL,ICNST,
     1                  IOFFP,SPVAL,BIGEST)
      DIMENSION       Z(MX,NY)   ,CL(NLM)
      DATA KK/0/
C
C CLSET PUTS THE VALUS OF THE CONTOUR LEVELS IN CL
C
      ICNST = 0
      GLO = CLO
      HA = CHI
      FANC = CINC
      CRAT = NLA
      IF (HA-GLO)  10, 20, 50
   10 GLO = HA
      HA = CLO
      GO TO  50
   20 GLO = BIGEST
      HA = -GLO
      DO  40 J=1,NY
         DO  30 I=1,NX
            IF (IOFFP.EQ.1 .AND. Z(I,J).EQ.SPVAL) GO TO  30
            GLO = MIN(Z(I,J),GLO)
            HA = MAX(Z(I,J),HA)
   30    CONTINUE
   40 CONTINUE
   50 IF (FANC)  60, 70, 90
   60 CRAT = -FANC
   70 FANC = (HA-GLO)/CRAT
      IF (FANC) 140,140, 80
   80 P = 10.**(INT(ALOG10(FANC)+500.)-500)
      FANC = REAL(INT(FANC/P))*P
   90 IF (CHI-CLO) 110,100,110
  100 GLO = REAL(INT(GLO/FANC))*FANC
      HA = REAL(INT(HA/FANC))*FANC
  110 DO 120 K=1,NLM
         CC = GLO+REAL(K-1)*FANC
         IF (CC .GT. HA) GO TO 130
         KK = K
         CL(K) = CC
  120 CONTINUE
  130 NCL = KK
      RETURN
  140 ICNST = 1
      RETURN
      END
