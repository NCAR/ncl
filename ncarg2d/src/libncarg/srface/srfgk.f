C
C $Id: srfgk.f,v 1.5 2008-07-27 00:17:26 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SRFGK (X,Y,Z,M,MX,NX,NY,S,STEREO)
C
      DIMENSION       X(NX)      ,Y(NY)      ,Z(MX,NY)   ,M(2,NX,NY) ,
     1                S(6)
      DIMENSION       MXS(2)     ,MXF(2)     ,MXJ(2)     ,MYS(2)     ,
     1                MYF(2)     ,MYJ(2)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX      ,
     1                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX       ,
     2                EYEY       ,EYEZ
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
     3                CLO        ,CINC       ,ISPVAL
      DATA JF,IF,LY,LX,ICNST/1,1,2,2,0/
      CALL Q8QST4 ('GRAPHX','SRFACE','SRFGK','VERSION 01')
      BIGEST = R1MACH(2)
      MMXX = MX
      NNXX = NX
      NNYY = NY
      STER = STEREO
      NXP1 = NNXX+1
      NYP1 = NNYY+1
      NLA = NCLA
      NSPVAL = ISPVAL
      NDRZ = IDRZ
      IF (IDRZ .NE. 0)
     1    CALL CLSET (Z,MMXX,NNXX,NNYY,CHI,CLO,CINC,NLA,40,CL,NCL,
     2                ICNST,IOFFP,SPVAL,BIGEST)
      IF (IDRZ .NE. 0) NDRZ = 1-ICNST
      STHETA = SIN(STER*THETA)
      CTHETA = COS(STER*THETA)
      RX = S(1)-S(4)
      RY = S(2)-S(5)
      RZ = S(3)-S(6)
      D1 = SQRT(RX*RX+RY*RY+RZ*RZ)
      D2 = SQRT(RX*RX+RY*RY)
      DX = 0.
      DY = 0.
      IF (STEREO .EQ. 0.) GO TO  20
      D1 = D1*STEREO*THETA
      IF (D2 .GT. 0.) GO TO  10
      DX = D1
      GO TO  20
   10 AGL = ATAN2(RX,-RY)
      DX = D1*COS(AGL)
      DY = D1*SIN(AGL)
   20 IROT = IROTS
      NPIC = 1
      IF (STER .NE. 0.) NPIC = 2
      FACT = 1.
      IF (NRSWT .NE. 0) FACT = RZERO/D1
      IF (ISTP.EQ.0 .AND. STER.NE.0.) IROT = 1
      DO 570 IPIC=1,NPIC
         NUPPER = IUPPER
         IF (IFR .LT. 0) CALL FRAME
C
C SET UP MAPPING FROM FLOATING POINT 3-SPACE TO CRT SPACE.
C
         SIGN1 = IPIC*2-3
         EYEX = S(1)+SIGN1*DX
         POIX = S(4)+SIGN1*DX
         EYEY = S(2)+SIGN1*DY
         POIY = S(5)+SIGN1*DY
         EYEZ = S(3)
         POIZ = S(6)
         LL = 0
         XEYE = EYEX
         YEYE = EYEY
         ZEYE = EYEZ
         CALL TRN32S (POIX,POIY,POIZ,XEYE,YEYE,ZEYE,0)
         LL = IPIC+2*ISTP+3
         IF (STER .EQ. 0.) LL = 1
         IF (NRSWT .NE. 0) GO TO 100
         XXMIN = X(1)
         XXMAX = X(NNXX)
         YYMIN = Y(1)
         YYMAX = Y(NNYY)
         UMIN = BIGEST
         VMIN = BIGEST
         ZZMIN = BIGEST
         UMAX = -UMIN
         VMAX = -VMIN
         ZZMAX = -ZZMIN
         DO  40 J=1,NNYY
            DO  30 I=1,NNXX
               ZZ = Z(I,J)
               IF (IOFFP.EQ.1 .AND. ZZ.EQ.SPVAL) GO TO  30
               ZZMAX = MAX(ZZMAX,ZZ)
               ZZMIN = MIN(ZZMIN,ZZ)
               CALL TRN32S (X(I),Y(J),Z(I,J),UT,VT,DUMMY,1)
               UMAX = MAX(UMAX,UT)
               UMIN = MIN(UMIN,UT)
               VMAX = MAX(VMAX,VT)
               VMIN = MIN(VMIN,VT)
   30       CONTINUE
   40    CONTINUE
         IF (ISKIRT .NE. 1) GO TO  70
         NXSTP = NNXX-1
         NYSTP = NNYY-1
         DO  60 J=1,NNYY,NYSTP
            DO  50 I=1,NNXX,NXSTP
               CALL TRN32S (X(I),Y(J),HSKIRT,UT,VT,DUMMY,1)
               UMAX = MAX(UMAX,UT)
               UMIN = MIN(UMIN,UT)
               VMAX = MAX(VMAX,VT)
               VMIN = MIN(VMIN,VT)
   50       CONTINUE
   60    CONTINUE
   70    CONTINUE
         WIDTH = UMAX-UMIN
         HIGHT = VMAX-VMIN
         DIF = .5*(WIDTH-HIGHT)
         IF (DIF)  80,100, 90
   80    UMIN = UMIN+DIF
         UMAX = UMAX-DIF
         GO TO 100
   90    VMIN = VMIN-DIF
         VMAX = VMAX+DIF
  100    XEYE = EYEX
         YEYE = EYEY
         ZEYE = EYEZ
         CALL TRN32S (POIX,POIY,POIZ,XEYE,YEYE,ZEYE,0)
         DO 120 J=1,NNYY
            DO 110 I=1,NNXX
               CALL TRN32S (X(I),Y(J),Z(I,J),UT,VT,DUMMY,1)
               M(1,I,J) = UT
               M(2,I,J) = VT
  110       CONTINUE
  120    CONTINUE
C
C INITIALIZE UPPER AND LOWER VISIBILITY ARRAYS
C
         DO 130 K=1,1024
            LIMU(K) = 0
            LIML(K) = 1024
  130    CONTINUE
C
C FIND ORDER TO DRAW LINES
C
         NXPASS = 1
         IF (S(1) .GE. X(NNXX)) GO TO 160
         IF (S(1) .LE. X(1)) GO TO 170
         DO 140 I=2,NNXX
            LX = I
            IF (S(1) .LE. X(I)) GO TO 150
  140    CONTINUE
  150    MXS(1) = LX-1
         MXJ(1) = -1
         MXF(1) = 1
         MXS(2) = LX
         MXJ(2) = 1
         MXF(2) = NNXX
         NXPASS = 2
         GO TO 180
  160    MXS(1) = NNXX
         MXJ(1) = -1
         MXF(1) = 1
         GO TO 180
  170    MXS(1) = 1
         MXJ(1) = 1
         MXF(1) = NNXX
  180    NYPASS = 1
         IF (S(2) .GE. Y(NNYY)) GO TO 210
         IF (S(2) .LE. Y(1)) GO TO 220
         DO 190 J=2,NNYY
            LY = J
            IF (S(2) .LE. Y(J)) GO TO 200
  190    CONTINUE
  200    MYS(1) = LY-1
         MYJ(1) = -1
         MYF(1) = 1
         MYS(2) = LY
         MYJ(2) = 1
         MYF(2) = NNYY
         NYPASS = 2
         GO TO 230
  210    MYS(1) = NNYY
         MYJ(1) = -1
         MYF(1) = 1
         GO TO 230
  220    MYS(1) = 1
         MYJ(1) = 1
         MYF(1) = NNYY
C
C PUT ON SKIRT ON FRONT SIDE IF WANTED
C
  230    IF (NXPASS.EQ.2 .AND. NYPASS.EQ.2) GO TO 490
         IF (ISKIRT .EQ. 0) GO TO 290
         IN = MXS(1)
         IF = MXF(1)
         JN = MYS(1)
         JF = MYF(1)
         IF (NYPASS .NE. 1) GO TO 260
         CALL TRN32S (X(1),Y(JN),HSKIRT,UX1,VX1,DUMMY,1)
         CALL TRN32S (X(NNXX),Y(JN),HSKIRT,UX2,VX2,DUMMY,1)
         QU = (UX2-UX1)/(X(NNXX)-X(1))
         QV = (VX2-VX1)/(X(NNXX)-X(1))
         YNOW = Y(JN)
         DO 240 I=1,NNXX
            CALL TRN32S (X(I),YNOW,HSKIRT,RU,RV,DUMMY,1)
            CALL DRAWS (INT(RU),INT(RV),M(1,I,JN),M(2,I,JN),1,1)
  240    CONTINUE
         CALL DRAWS (INT(UX1),INT(VX1),INT(UX2),INT(VX2),1,1)
         IF (IDRY .NE. 0) GO TO 260
         DO 250 I=2,NNXX
            CALL DRAWS (M(1,I-1,JN),M(2,I-1,JN),M(1,I,JN),M(2,I,JN),1,1)
  250    CONTINUE
  260    IF (NXPASS .NE. 1) GO TO 290
         CALL TRN32S (X(IN),Y(1),HSKIRT,UY1,VY1,DUMMY,1)
         CALL TRN32S (X(IN),Y(NNYY),HSKIRT,UY2,VY2,DUMMY,1)
         QU = (UY2-UY1)/(Y(NNYY)-Y(1))
         QV = (VY2-VY1)/(Y(NNYY)-Y(1))
         XNOW = X(IN)
         DO 270 J=1,NNYY
            CALL TRN32S (XNOW,Y(J),HSKIRT,RU,RV,DUMMY,1)
            CALL DRAWS (INT(RU),INT(RV),M(1,IN,J),M(2,IN,J),1,1)
  270    CONTINUE
         CALL DRAWS (INT(UY1),INT(VY1),INT(UY2),INT(VY2),1,1)
         IF (IDRX .NE. 0) GO TO 290
         DO 280 J=2,NNYY
            CALL DRAWS (M(1,IN,J-1),M(2,IN,J-1),M(1,IN,J),M(2,IN,J),1,1)
  280    CONTINUE
C
C PICK PROPER ALGORITHM
C
  290    LI = MXJ(1)
         MI = MXS(1)-LI
         NI = ABS(MI-MXF(1))
         LJ = MYJ(1)
         MJ = MYS(1)-LJ
         NJ = ABS(MJ-MYF(1))
C
C IF NXPASS IS 1, PUT THE I LOOP OUTERMOST.  OTHERWISE, PUT THE J LOOP
C OUTERMOST.
C
         IF (NXPASS.EQ.2) GO TO 360
         IF (ISKIRT.NE.0 .OR. NYPASS.NE.1) GO TO 310
         I = MXS(1)
         DO 300 J=2,NNYY
            CALL DRAWS (M(1,I,J-1),M(2,I,J-1),M(1,I,J),M(2,I,J),0,1)
  300    CONTINUE
  310    DO 350 II=1,NNXX
            I = MI+II*LI
            IPLI = I+LI
            IF (NYPASS .EQ. 1) GO TO 320
            K = MYS(1)
            L = MYS(2)
            IF (IDRX .NE. 0)
     1          CALL DRAWS (M(1,I,K),M(2,I,K),M(1,I,L),M(2,I,L),1,1)
            IF (NDRZ.NE.0 .AND. II.NE.NI)
     1          CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN(I,I+LI),K)
  320       DO 340 JPASS=1,NYPASS
               LJ = MYJ(JPASS)
               MJ = MYS(JPASS)-LJ
               NJ = ABS(MJ-MYF(JPASS))
               DO 330 JJ=1,NJ
                  J = MJ+JJ*LJ
                  JPLJ = J+LJ
                  IF (IDRX.NE.0 .AND. JJ.NE.NJ)
     1                CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I,JPLJ),
     2                            M(2,I,JPLJ),1,1)
                  IF (I.NE.MXF(1) .AND. IDRY.NE.0)
     1                CALL DRAWS (M(1,IPLI,J),M(2,IPLI,J),M(1,I,J),
     2                            M(2,I,J),1,1)
                  IF (NDRZ.NE.0 .AND. JJ.NE.NJ .AND. II.NE.NNXX)
     1                CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN(I,I+LI),
     2                             MIN(J,J+LJ))
  330          CONTINUE
  340       CONTINUE
  350    CONTINUE
         GO TO 430
  360    IF (ISKIRT.NE.0 .OR. NXPASS.NE.1) GO TO 380
         J = MYS(1)
         DO 370 I=2,NNXX
            CALL DRAWS (M(1,I-1,J),M(2,I-1,J),M(1,I,J),M(2,I,J),0,1)
  370    CONTINUE
  380    DO 420 JJ=1,NNYY
            J = MJ+JJ*LJ
            JPLJ = J+LJ
            IF (NXPASS .EQ. 1) GO TO 390
            K = MXS(1)
            L = MXS(2)
            IF (IDRY .NE. 0)
     1          CALL DRAWS (M(1,K,J),M(2,K,J),M(1,L,J),M(2,L,J),1,1)
            IF (NDRZ.NE.0 .AND. JJ.NE.NJ)
     1          CALL CTCELL (Z,MMXX,NNXX,NNYY,M,K,MIN(J,J+LJ))
  390       DO 410 IPASS=1,NXPASS
               LI = MXJ(IPASS)
               MI = MXS(IPASS)-LI
               NI = ABS(MI-MXF(IPASS))
               DO 400 II=1,NI
                  I = MI+II*LI
                  IPLI = I+LI
                  IF (IDRY.NE.0 .AND. II.NE.NI)
     1                CALL DRAWS (M(1,I,J),M(2,I,J),M(1,IPLI,J),
     2                            M(2,IPLI,J),1,1)
                  IF (J.NE.MYF(1) .AND. IDRX.NE.0)
     1                CALL DRAWS (M(1,I,JPLJ),M(2,I,JPLJ),M(1,I,J),
     2                            M(2,I,J),1,1)
                  IF (NDRZ.NE.0 .AND. II.NE.NI .AND. JJ.NE.NNYY)
     1                CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN(I,I+LI),
     2                             MIN(J,J+LJ))
  400          CONTINUE
  410       CONTINUE
  420    CONTINUE
  430    IF (ISKIRT .EQ. 0) GO TO 520
C
C FIX UP IF SKIRT IS USED WITH LINES ONE WAY.
C
         IF (IDRX .NE. 0) GO TO 460
         DO 450 IPASS=1,NXPASS
            IF (NXPASS .EQ. 2) IF = 1+(IPASS-1)*(NNXX-1)
            DO 440 J=2,NNYY
               CALL DRAWS (M(1,IF,J-1),M(2,IF,J-1),M(1,IF,J),M(2,IF,J),
     1                     1,0)
  440       CONTINUE
  450    CONTINUE
  460    IF (IDRY .NE. 0) GO TO 520
         DO 480 JPASS=1,NYPASS
            IF (NYPASS .EQ. 2) JF = 1+(JPASS-1)*(NNYY-1)
            DO 470 I=2,NNXX
               CALL DRAWS (M(1,I-1,JF),M(2,I-1,JF),M(1,I,JF),M(2,I,JF),
     1                     1,0)
  470       CONTINUE
  480    CONTINUE
         GO TO 520
C
C ALL VISIBLE IF VIEWED FROM DIRECTLY ABOVE OR BELOW.
C
  490    IF (NUPPER.GT.0 .AND. S(3).LT.S(6)) GO TO 520
         IF (NUPPER.LT.0 .AND. S(3).GT.S(6)) GO TO 520
         NUPPER = 1
         IF (S(3) .LT. S(6)) NUPPER = -1
         DO 510 I=1,NNXX
            DO 500 J=1,NNYY
               IF (IDRX.NE.0 .AND. J.NE.NNYY)
     1             CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I,J+1),M(2,I,J+1),
     2                         1,0)
               IF (IDRY.NE.0 .AND. I.NE.NNXX)
     1             CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I+1,J),M(2,I+1,J),
     2                         1,0)
               IF (IDRZ.NE.0 .AND. I.NE.NNXX .AND. J.NE.NNYY)
     1             CALL CTCELL (Z,MMXX,NNXX,NNYY,M,I,J)
  500       CONTINUE
  510    CONTINUE
  520    IF (STER .EQ. 0.) GO TO 560
         IF (ISTP) 540,530,550
  530    CALL FRAME
  540    CALL FRAME
         GO TO 570
  550    IF (IPIC .NE. 2) GO TO 570
  560    IF (IFR .GT. 0) CALL FRAME
  570 CONTINUE
      RETURN
      END
