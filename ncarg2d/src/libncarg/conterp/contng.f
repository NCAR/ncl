C
C	$Id: contng.f,v 1.4 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONTNG (NDP,XD,YD,NT,IPT,NL,IPL,IWL,IWP,WK)
C
C THIS SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y
C PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA
C POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE
C BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS
C CORRESPONDING TO THE BORDER LINE SEGMENTS.
C AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS
C OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,
C LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           X COORDINATES OF THE DATA POINTS,
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           Y COORDINATES OF THE DATA POINTS.
C THE OUTPUT PARAMETERS ARE
C     NT  = NUMBER OF TRIANGLES,
C     IPT = ARRAY OF DIMENSION 6*NDP-15, WHERE THE POINT
C           NUMBERS OF THE VERTEXES OF THE (IT)TH TRIANGLE
C           ARE TO BE STORED AS THE (3*IT-2)ND, (3*IT-1)ST,
C           AND (3*IT)TH ELEMENTS, IT=1,2,...,NT,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = ARRAY OF DIMENSION 6*NDP, WHERE THE POINT
C           NUMBERS OF THE END POINTS OF THE (IL)TH BORDER
C           LINE SEGMENT AND ITS RESPECTIVE TRIANGLE NUMBER
C           ARE TO BE STORED AS THE (3*IL-2)ND, (3*IL-1)ST,
C           AND (3*IL)TH ELEMENTS, IL=1,2,..., NL.
C THE OTHER PARAMETERS ARE
C     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED
C           INTERNALLY AS A WORK AREA,
C     IWP = INTEGER ARRAY OF DIMENSION NDP USED
C           INTERNALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
C           WORK AREA.
C DECLARATION STATEMENTS
C
      SAVE
C
      INTEGER         CONXCH
      COMMON /CONRA3/ IREC
      DIMENSION       XD(*)      ,YD(*)      ,IPT(*)     ,IPL(*)     ,
     1                IWL(*)     ,IWP(*)     ,WK(*)
      DIMENSION       ITF(2)
        CHARACTER*4  IP1C, IP2C
        CHARACTER*64  ITEMP
      DATA  RATIO/1.0E-6/, NREP/100/
C
C STATEMENT FUNCTIONS
C
      DSQF(U1,V1,U2,V2) = (U2-U1)**2+(V2-V1)**2
      SIDE(U1,V1,U2,V2,U3,V3) = (V3-V1)*(U2-U1)-(U3-U1)*(V2-V1)
C
C PRELIMINARY PROCESSING
C
      NDPM1 = NDP-1
C
C DETERMINES THE CLOSEST PAIR OF DATA POINTS AND THEIR MIDPOINT.
C
      DSQMN = DSQF(XD(1),YD(1),XD(2),YD(2))
      IPMN1 = 1
      IPMN2 = 2
      DO  140 IP1=1,NDPM1
         X1 = XD(IP1)
         Y1 = YD(IP1)
         IP1P1 = IP1+1
         DO  130 IP2=IP1P1,NDP
            DSQI = DSQF(X1,Y1,XD(IP2),YD(IP2))
            IF (DSQI .NE. 0.) GO TO  120
C
C  ERROR, IDENTICAL INPUT DATA POINTS
C
          ITEMP = ' CONTNG-IDENTICAL INPUT DATA POINTS FOUND
     1 AT      AND      '
          WRITE(IP1C,'(I4)')IP1
            WRITE(IP2C,'(I4)')IP2
            CALL SETER (ITEMP,1,1)
            ITEMP(46:49) = IP1C
            ITEMP(55:58) = IP2C
            RETURN
  120       IF (DSQI .GE. DSQMN) GO TO  130
            DSQMN = DSQI
            IPMN1 = IP1
            IPMN2 = IP2
  130    CONTINUE
  140 CONTINUE
      DSQ12 = DSQMN
      XDMP = (XD(IPMN1)+XD(IPMN2))/2.0
      YDMP = (YD(IPMN1)+YD(IPMN2))/2.0
C
C SORTS THE OTHER (NDP-2) DATA POINTS IN ASCENDING ORDER OF
C DISTANCE FROM THE MIDPOINT AND STORES THE SORTED DATA POINT
C NUMBERS IN THE IWP ARRAY.
C
      JP1 = 2
      DO  150 IP1=1,NDP
         IF (IP1.EQ.IPMN1 .OR. IP1.EQ.IPMN2) GO TO  150
         JP1 = JP1+1
         IWP(JP1) = IP1
         WK(JP1) = DSQF(XDMP,YDMP,XD(IP1),YD(IP1))
  150 CONTINUE
      DO  170 JP1=3,NDPM1
         DSQMN = WK(JP1)
         JPMN = JP1
         DO  160 JP2=JP1,NDP
            IF (WK(JP2) .GE. DSQMN) GO TO  160
            DSQMN = WK(JP2)
            JPMN = JP2
  160    CONTINUE
         ITS = IWP(JP1)
         IWP(JP1) = IWP(JPMN)
         IWP(JPMN) = ITS
         WK(JPMN) = WK(JP1)
  170 CONTINUE
C
C IF NECESSARY, MODIFIES THE ORDERING IN SUCH A WAY THAT THE
C FIRST THREE DATA POINTS ARE NOT COLLINEAR.
C
      AR = DSQ12*RATIO
      X1 = XD(IPMN1)
      Y1 = YD(IPMN1)
      DX21 = XD(IPMN2)-X1
      DY21 = YD(IPMN2)-Y1
      DO  180 JP=3,NDP
         IP = IWP(JP)
         IF (ABS((YD(IP)-Y1)*DX21-(XD(IP)-X1)*DY21) .GT. AR) GO TO  190
  180 CONTINUE
      CALL SETER (' CONTNG - ALL COLLINEAR DATA POINTS',1,1)
  190 IF (JP .EQ. 3) GO TO  210
      JPMX = JP
      JP = JPMX+1
      DO  200 JPC=4,JPMX
         JP = JP-1
         IWP(JP) = IWP(JP-1)
  200 CONTINUE
      IWP(3) = IP
C
C FORMS THE FIRST TRIANGLE.  STORES POINT NUMBERS OF THE VER-
C TEXES OF THE TRIANGLE IN THE IPT ARRAY, AND STORES POINT NUM-
C BERS OF THE BORDER LINE SEGMENTS AND THE TRIANGLE NUMBER IN
C THE IPL ARRAY.
C
  210 IP1 = IPMN1
      IP2 = IPMN2
      IP3 = IWP(3)
      IF (SIDE(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3)) .GE.
     1    0.0) GO TO  220
      IP1 = IPMN2
      IP2 = IPMN1
  220 NT0 = 1
      NTT3 = 3
      IPT(1) = IP1
      IPT(2) = IP2
      IPT(3) = IP3
      NL0 = 3
      NLT3 = 9
      IPL(1) = IP1
      IPL(2) = IP2
      IPL(3) = 1
      IPL(4) = IP2
      IPL(5) = IP3
      IPL(6) = 1
      IPL(7) = IP3
      IPL(8) = IP1
      IPL(9) = 1
C
C ADDS THE REMAINING (NDP-3) DATA POINTS, ONE BY ONE.
C
      DO  400 JP1=4,NDP
         IP1 = IWP(JP1)
         X1 = XD(IP1)
         Y1 = YD(IP1)
C
C - DETERMINES THE VISIBLE BORDER LINE SEGMENTS.
C
         IP2 = IPL(1)
         JPMN = 1
         DXMN = XD(IP2)-X1
         DYMN = YD(IP2)-Y1
         DSQMN = DXMN**2+DYMN**2
         ARMN = DSQMN*RATIO
         JPMX = 1
         DXMX = DXMN
         DYMX = DYMN
         DSQMX = DSQMN
         ARMX = ARMN
         DO  240 JP2=2,NL0
            IP2 = IPL(3*JP2-2)
            DX = XD(IP2)-X1
            DY = YD(IP2)-Y1
            AR = DY*DXMN-DX*DYMN
            IF (AR .GT. ARMN) GO TO  230
            DSQI = DX**2+DY**2
            IF (AR.GE.(-ARMN) .AND. DSQI.GE.DSQMN) GO TO  230
            JPMN = JP2
            DXMN = DX
            DYMN = DY
            DSQMN = DSQI
            ARMN = DSQMN*RATIO
  230       AR = DY*DXMX-DX*DYMX
            IF (AR .LT. (-ARMX)) GO TO  240
            DSQI = DX**2+DY**2
            IF (AR.LE.ARMX .AND. DSQI.GE.DSQMX) GO TO  240
            JPMX = JP2
            DXMX = DX
            DYMX = DY
            DSQMX = DSQI
            ARMX = DSQMX*RATIO
  240    CONTINUE
         IF (JPMX .LT. JPMN) JPMX = JPMX+NL0
         NSH = JPMN-1
         IF (NSH .LE. 0) GO TO  270
C
C - SHIFTS (ROTATES) THE IPL ARRAY TO HAVE THE INVISIBLE BORDER
C - LINE SEGMENTS CONTAINED IN THE FIRST PART OF THE IPL ARRAY.
C
         NSHT3 = NSH*3
         DO  250 JP2T3=3,NSHT3,3
            JP3T3 = JP2T3+NLT3
            IPL(JP3T3-2) = IPL(JP2T3-2)
            IPL(JP3T3-1) = IPL(JP2T3-1)
            IPL(JP3T3) = IPL(JP2T3)
  250    CONTINUE
         DO  260 JP2T3=3,NLT3,3
            JP3T3 = JP2T3+NSHT3
            IPL(JP2T3-2) = IPL(JP3T3-2)
            IPL(JP2T3-1) = IPL(JP3T3-1)
            IPL(JP2T3) = IPL(JP3T3)
  260    CONTINUE
         JPMX = JPMX-NSH
C
C - ADDS TRIANGLES TO THE IPT ARRAY, UPDATES BORDER LINE
C - SEGMENTS IN THE IPL ARRAY, AND SETS FLAGS FOR THE BORDER
C - LINE SEGMENTS TO BE REEXAMINED IN THE IWL ARRAY.
C
  270    JWL = 0
         DO  310 JP2=JPMX,NL0
            JP2T3 = JP2*3
            IPL1 = IPL(JP2T3-2)
            IPL2 = IPL(JP2T3-1)
            IT = IPL(JP2T3)
C
C - - ADDS A TRIANGLE TO THE IPT ARRAY.
C
            NT0 = NT0+1
            NTT3 = NTT3+3
            IPT(NTT3-2) = IPL2
            IPT(NTT3-1) = IPL1
            IPT(NTT3) = IP1
C
C - - UPDATES BORDER LINE SEGMENTS IN THE IPL ARRAY.
C
            IF (JP2 .NE. JPMX) GO TO  280
            IPL(JP2T3-1) = IP1
            IPL(JP2T3) = NT0
  280       IF (JP2 .NE. NL0) GO TO  290
            NLN = JPMX+1
            NLNT3 = NLN*3
            IPL(NLNT3-2) = IP1
            IPL(NLNT3-1) = IPL(1)
            IPL(NLNT3) = NT0
C
C - - DETERMINES THE VERTEX THAT DOES NOT LIE ON THE BORDER
C - - LINE SEGMENTS.
C
  290       ITT3 = IT*3
            IPTI = IPT(ITT3-2)
            IF (IPTI.NE.IPL1 .AND. IPTI.NE.IPL2) GO TO  300
            IPTI = IPT(ITT3-1)
            IF (IPTI.NE.IPL1 .AND. IPTI.NE.IPL2) GO TO  300
            IPTI = IPT(ITT3)
C
C - - CHECKS IF THE EXCHANGE IS NECESSARY.
C
  300       IF (CONXCH(XD,YD,IP1,IPTI,IPL1,IPL2) .EQ. 0) GO TO  310
C
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
C
            IPT(ITT3-2) = IPTI
            IPT(ITT3-1) = IPL1
            IPT(ITT3) = IP1
            IPT(NTT3-1) = IPTI
            IF (JP2 .EQ. JPMX) IPL(JP2T3) = IT
            IF (JP2.EQ.NL0 .AND. IPL(3).EQ.IT) IPL(3) = NT0
C
C - - SETS FLAGS IN THE IWL ARRAY.
C
            JWL = JWL+4
            IWL(JWL-3) = IPL1
            IWL(JWL-2) = IPTI
            IWL(JWL-1) = IPTI
            IWL(JWL) = IPL2
  310    CONTINUE
         NL0 = NLN
         NLT3 = NLNT3
         NLF = JWL/2
         IF (NLF .EQ. 0) GO TO  400
C
C - IMPROVES TRIANGULATION.
C
         NTT3P3 = NTT3+3
         DO  390 IREP=1,NREP
            DO  370 ILF=1,NLF
               ILFT2 = ILF*2
               IPL1 = IWL(ILFT2-1)
               IPL2 = IWL(ILFT2)
C
C - - LOCATES IN THE IPT ARRAY TWO TRIANGLES ON BOTH SIDES OF
C - - THE FLAGGED LINE SEGMENT.
C
               NTF = 0
               DO  320 ITT3R=3,NTT3,3
                  ITT3 = NTT3P3-ITT3R
                  IPT1 = IPT(ITT3-2)
                  IPT2 = IPT(ITT3-1)
                  IPT3 = IPT(ITT3)
                  IF (IPL1.NE.IPT1 .AND. IPL1.NE.IPT2 .AND.
     1                IPL1.NE.IPT3) GO TO  320
                  IF (IPL2.NE.IPT1 .AND. IPL2.NE.IPT2 .AND.
     1                IPL2.NE.IPT3) GO TO  320
                  NTF = NTF+1
                  ITF(NTF) = ITT3/3
                  IF (NTF .EQ. 2) GO TO  330
  320          CONTINUE
               IF (NTF .LT. 2) GO TO  370
C
C - - DETERMINES THE VERTEXES OF THE TRIANGLES THAT DO NOT LIE
C - - ON THE LINE SEGMENT.
C
  330          IT1T3 = ITF(1)*3
               IPTI1 = IPT(IT1T3-2)
               IF (IPTI1.NE.IPL1 .AND. IPTI1.NE.IPL2) GO TO  340
               IPTI1 = IPT(IT1T3-1)
               IF (IPTI1.NE.IPL1 .AND. IPTI1.NE.IPL2) GO TO  340
               IPTI1 = IPT(IT1T3)
  340          IT2T3 = ITF(2)*3
               IPTI2 = IPT(IT2T3-2)
               IF (IPTI2.NE.IPL1 .AND. IPTI2.NE.IPL2) GO TO  350
               IPTI2 = IPT(IT2T3-1)
               IF (IPTI2.NE.IPL1 .AND. IPTI2.NE.IPL2) GO TO  350
               IPTI2 = IPT(IT2T3)
C
C - - CHECKS IF THE EXCHANGE IS NECESSARY.
C
  350          IF (CONXCH(XD,YD,IPTI1,IPTI2,IPL1,IPL2) .EQ. 0)
     1             GO TO  370
C
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
C
               IPT(IT1T3-2) = IPTI1
               IPT(IT1T3-1) = IPTI2
               IPT(IT1T3) = IPL1
               IPT(IT2T3-2) = IPTI2
               IPT(IT2T3-1) = IPTI1
               IPT(IT2T3) = IPL2
C
C - - SETS NEW FLAGS.
C
               JWL = JWL+8
               IWL(JWL-7) = IPL1
               IWL(JWL-6) = IPTI1
               IWL(JWL-5) = IPTI1
               IWL(JWL-4) = IPL2
               IWL(JWL-3) = IPL2
               IWL(JWL-2) = IPTI2
               IWL(JWL-1) = IPTI2
               IWL(JWL) = IPL1
               DO  360 JLT3=3,NLT3,3
                  IPLJ1 = IPL(JLT3-2)
                  IPLJ2 = IPL(JLT3-1)
                  IF ((IPLJ1.EQ.IPL1 .AND. IPLJ2.EQ.IPTI2) .OR.
     1                (IPLJ2.EQ.IPL1 .AND. IPLJ1.EQ.IPTI2))
     2                IPL(JLT3) = ITF(1)
                  IF ((IPLJ1.EQ.IPL2 .AND. IPLJ2.EQ.IPTI1) .OR.
     1                (IPLJ2.EQ.IPL2 .AND. IPLJ1.EQ.IPTI1))
     2                IPL(JLT3) = ITF(2)
  360          CONTINUE
  370       CONTINUE
            NLFC = NLF
            NLF = JWL/2
            IF (NLF .EQ. NLFC) GO TO  400
C
C - - RESETS THE IWL ARRAY FOR THE NEXT ROUND.
C
            JWL = 0
            JWL1MN = (NLFC+1)*2
            NLFT2 = NLF*2
            DO  380 JWL1=JWL1MN,NLFT2,2
               JWL = JWL+2
               IWL(JWL-1) = IWL(JWL1-1)
               IWL(JWL) = IWL(JWL1)
  380       CONTINUE
            NLF = JWL/2
  390    CONTINUE
  400 CONTINUE
C
C REARRANGE THE IPT ARRAY SO THAT THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.
C
      DO  410 ITT3=3,NTT3,3
         IP1 = IPT(ITT3-2)
         IP2 = IPT(ITT3-1)
         IP3 = IPT(ITT3)
         IF (SIDE(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3)) .GE.
     1       0.0) GO TO  410
         IPT(ITT3-2) = IP2
         IPT(ITT3-1) = IP1
  410 CONTINUE
      NT = NT0
      NL = NL0
      RETURN
      END
