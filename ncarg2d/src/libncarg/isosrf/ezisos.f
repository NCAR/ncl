C
C $Id: ezisos.f,v 1.5 2008-07-27 00:17:15 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE EZISOS (T,MU,MV,MW,EYE,SLAB,TISO)
C
C Arguments are as described in ISOSRF.
C
      DIMENSION T(MU,MV,MW),EYE(3)
C
      DATA ANG,PI / .35 , 3.14159265358979 /
C
C Pick types of lines to draw.
C
      NU = MU
      NV = MV
      NW = MW
      TVAL = TISO
      MAXN = MAX(NU,NV,NW)+2
      ATU = NU/2
      ATV = NV/2
      ATW = NW/2
      EYEU = EYE(1)
      EYEV = EYE(2)
      EYEW = EYE(3)
      RU = EYEU-ATU
      RV = EYEV-ATV
      RW = EYEW-ATW
      RU2 = RU*RU
      RV2 = RV*RV
      RW2 = RW*RW
      DU = SQRT(RV2+RW2)
      DV = SQRT(RU2+RW2)
      DW = SQRT(RU2+RV2)
      DR = 1./SQRT(RU2+RV2+RW2)
C
C Compute the arc cosine.
C
      TU = DU*DR
      ANGU = ATAN(ABS(SQRT(1.-TU*TU)/TU))
      IF (TU .LE. 0.) ANGU = PI-ANGU
      TV = DV*DR
      ANGV = ATAN(ABS(SQRT(1.-TV*TV)/TV))
      IF (TV .LE. 0.) ANGV = PI-ANGV
      TW = DW*DR
      ANGW = ATAN(ABS(SQRT(1.-TW*TW)/TW))
      IF (TW .LE. 0.) ANGW = PI-ANGW
C
C Breakpoint is about 20 degrees or about .35 radians.
C
      IFLAG = 0
      IF (ANGU .GT. ANG) IFLAG = IFLAG+4
      IF (ANGV .GT. ANG) IFLAG = IFLAG+2
      IF (ANGW .GT. ANG) IFLAG = IFLAG+1
C
C Find sign of IFLAG.
C
      ICNT = 0
      IF (ABS(RU) .LE. ATU) GO TO  30
      IU = 1
      IF (EYEU .GT. ATU) IU = NU
      DO  20 IW=1,NW
         DO  10 IV=1,NV
            IF (T(IU,IV,IW) .GT. TVAL) ICNT = ICNT-2
            ICNT = ICNT+1
   10    CONTINUE
   20 CONTINUE
   30 IF (ABS(RV) .LE. ATV) GO TO  60
      IV = 1
      IF (EYEV .GT. ATV) IV = NV
      DO  50 IW=1,NW
         DO  40 IU=1,NU
            IF (T(IU,IV,IW) .GT. TVAL) ICNT = ICNT-2
            ICNT = ICNT+1
   40    CONTINUE
   50 CONTINUE
   60 IF (ABS(RW) .LE. ATW) GO TO  90
      IW = 1
      IF (EYEW .GT. ATW) IW = NW
      DO  80 IV=1,NV
         DO  70 IU=1,NU
            IF (T(IU,IV,IW) .GT. TVAL) ICNT = ICNT-2
            ICNT = ICNT+1
   70    CONTINUE
   80 CONTINUE
   90 IFLAG = SIGN(IFLAG,ICNT)
      CALL ISOSRF (T,NU,NU,NV,NV,NW,EYE,MAXN,SLAB,TVAL,IFLAG)
      CALL FRAME
      RETURN
      END
