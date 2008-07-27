C
C	$Id: concal.f,v 1.4 2008-07-27 00:16:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONCAL (XD,YD,ZD,NT,IPT,NL,IPL,PDD,ITI,XII,YII,ZII,
     1                   ITPV)
C
C THIS SUBROUTINE PERFORMS PUNCTUAL INTERPOLATION OR EXTRAPO-
C LATION, I.E., DETERMINES THE Z VALUE AT A POINT.
C THE INPUT PARAMETERS ARE
C
C     XD,YD,ZD = ARRAYS CONTAINING THE X, Y, AND Z
C                COORDINATES OF DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY CONTAINING THE POINT NUMBERS OF
C           THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY CONTAINING THE POINT NUMBERS OF
C           THE END POINTS OF THE BORDER LINE SEGMENTS AND
C           THEIR RESPECTIVE TRIANGLE NUMBERS,
C     PDD = ARRAY CONTAINING THE PARTIAL DERIVATIVES AT
C           THE DATA POINTS,
C     ITI = TRIANGLE NUMBER OF THE TRIANGLE IN WHICH LIES
C           THE POINT FOR WHICH INTERPOLATION IS TO BE
C           PERFORMED,
C     XII,YII = X AND Y COORDINATES OF THE POINT FOR WHICH
C               INTERPOLATION IS TO BE PERFORMED.
C THE OUTPUT PARAMETER IS
C
C     ZII = INTERPOLATED Z VALUE.
C
C DECLARATION STATEMENTS
C
C
      DIMENSION       XD(1)      ,YD(1)      ,ZD(1)      ,IPT(1)     ,
     1                IPL(1)     ,PDD(1)
      DIMENSION       X(3)       ,Y(3)       ,Z(3)       ,PD(15)     ,
     1                ZU(3)      ,ZV(3)      ,ZUU(3)     ,ZUV(3)     ,
     2                ZVV(3)
      REAL            LU         ,LV
      EQUIVALENCE     (P5,P50)
C
        SAVE
C
C PRELIMINARY PROCESSING
C
      IT0 = ITI
      NTL = NT+NL
      IF (IT0 .LE. NTL) GO TO  100
      IL1 = IT0/NTL
      IL2 = IT0-IL1*NTL
      IF (IL1 .EQ. IL2) GO TO  150
      GO TO  200
C
C CALCULATION OF ZII BY INTERPOLATION.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
C
  100 IF (IT0 .EQ. ITPV) GO TO  140
C
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE
C IPI 102 VERTEXES.
C IPI 103
C
      JIPT = 3*(IT0-1)
      JPD = 0
      DO  120 I=1,3
         JIPT = JIPT+1
         IDP = IPT(JIPT)
         X(I) = XD(IDP)
         Y(I) = YD(IDP)
         Z(I) = ZD(IDP)
         JPDD = 5*(IDP-1)
         DO  110 KPD=1,5
            JPD = JPD+1
            JPDD = JPDD+1
            PD(JPD) = PDD(JPDD)
  110    CONTINUE
  120 CONTINUE
C
C DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
C TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
C AND VICE VERSA.
C
      X0 = X(1)
      Y0 = Y(1)
      A = X(2)-X0
      B = X(3)-X0
      C = Y(2)-Y0
      D = Y(3)-Y0
      AD = A*D
      BC = B*C
      DLT = AD-BC
      AP = D/DLT
      BP = -B/DLT
      CP = -C/DLT
      DP = A/DLT
C
C CONVERTS THE PARTIAL DERIVATIVES AT THE VERTEXES OF THE
C TRIANGLE FOR THE U-V COORDINATE SYSTEM.
C
      AA = A*A
      ACT2 = 2.0*A*C
      CC = C*C
      AB = A*B
      ADBC = AD+BC
      CD = C*D
      BB = B*B
      BDT2 = 2.0*B*D
      DD = D*D
      DO  130 I=1,3
         JPD = 5*I
         ZU(I) = A*PD(JPD-4)+C*PD(JPD-3)
         ZV(I) = B*PD(JPD-4)+D*PD(JPD-3)
         ZUU(I) = AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
         ZUV(I) = AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
         ZVV(I) = BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
  130 CONTINUE
C
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
C
      P00 = Z(1)
      P10 = ZU(1)
      P01 = ZV(1)
      P20 = 0.5*ZUU(1)
      P11 = ZUV(1)
      P02 = 0.5*ZVV(1)
      H1 = Z(2)-P00-P10-P20
      H2 = ZU(2)-P10-ZUU(1)
      H3 = ZUU(2)-ZUU(1)
      P30 = 10.0*H1-4.0*H2+0.5*H3
      P40 = -15.0*H1+7.0*H2-H3
      P50 = 6.0*H1-3.0*H2+0.5*H3
      H1 = Z(3)-P00-P01-P02
      H2 = ZV(3)-P01-ZVV(1)
      H3 = ZVV(3)-ZVV(1)
      P03 = 10.0*H1-4.0*H2+0.5*H3
      P04 = -15.0*H1+7.0*H2-H3
      P05 = 6.0*H1-3.0*H2+0.5*H3
      LU = SQRT(AA+CC)
      LV = SQRT(BB+DD)
      THXU = ATAN2(C,A)
      THUV = ATAN2(D,B)-THXU
      CSUV = COS(THUV)
      P41 = 5.0*LV*CSUV/LU*P50
      P14 = 5.0*LU*CSUV/LV*P05
      H1 = ZV(2)-P01-P11-P41
      H2 = ZUV(2)-P11-4.0*P41
      P21 = 3.0*H1-H2
      P31 = -2.0*H1+H2
      H1 = ZU(3)-P10-P11-P14
      H2 = ZUV(3)-P11-4.0*P14
      P12 = 3.0*H1-H2
      P13 = -2.0*H1+H2
      THUS = ATAN2(D-C,B-A)-THXU
      THSV = THUV-THUS
      AA = SIN(THSV)/LU
      BB = -COS(THSV)/LU
      CC = SIN(THUS)/LV
      DD = COS(THUS)/LV
      AC = AA*CC
      AD = AA*DD
      BC = BB*CC
      G1 = AA*AC*(3.0*BC+2.0*AD)
      G2 = CC*AC*(3.0*AD+2.0*BC)
      H1 = -AA*AA*AA*(5.0*AA*BB*P50+(4.0*BC+AD)*P41)-
     1     CC*CC*CC*(5.0*CC*DD*P05+(4.0*AD+BC)*P14)
      H2 = 0.5*ZVV(2)-P02-P12
      H3 = 0.5*ZUU(3)-P20-P21
      P22 = (G1*H2+G2*H3-H1)/(G1+G2)
      P32 = H2-P22
      P23 = H3-P22
      ITPV = IT0
C
C CONVERTS XII AND YII TO U-V SYSTEM.
C
  140 DX = XII-X0
      DY = YII-Y0
      U = AP*DX+BP*DY
      V = CP*DX+DP*DY
C
C EVALUATES THE POLYNOMIAL.
C
      P0 = P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1 = P10+V*(P11+V*(P12+V*(P13+V*P14)))
      P2 = P20+V*(P21+V*(P22+V*P23))
      P3 = P30+V*(P31+V*P32)
      P4 = P40+V*P41
      ZII = P0+U*(P1+U*(P2+U*(P3+U*(P4+U*P5))))
      RETURN
C
C CALCULATION OF ZII BY EXTRATERPOLATION IN THE RECTANGLE.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
C
  150 IF (IT0 .EQ. ITPV) GO TO  190
C
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE END
C POINTS OF THE BORDER LINE SEGMENT.
C
      JIPL = 3*(IL1-1)
      JPD = 0
      DO  170 I=1,2
         JIPL = JIPL+1
         IDP = IPL(JIPL)
         X(I) = XD(IDP)
         Y(I) = YD(IDP)
         Z(I) = ZD(IDP)
         JPDD = 5*(IDP-1)
         DO  160 KPD=1,5
            JPD = JPD+1
            JPDD = JPDD+1
            PD(JPD) = PDD(JPDD)
  160    CONTINUE
  170 CONTINUE
C
C DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
C TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
C AND VICE VERSA.
C
      X0 = X(1)
      Y0 = Y(1)
      A = Y(2)-Y(1)
      B = X(2)-X(1)
      C = -B
      D = A
      AD = A*D
      BC = B*C
      DLT = AD-BC
      AP = D/DLT
      BP = -B/DLT
      CP = -BP
      DP = AP
C
C CONVERTS THE PARTIAL DERIVATIVES AT THE END POINTS OF THE
C BORDER LINE SEGMENT FOR THE U-V COORDINATE SYSTEM.
C
      AA = A*A
      ACT2 = 2.0*A*C
      CC = C*C
      AB = A*B
      ADBC = AD+BC
      CD = C*D
      BB = B*B
      BDT2 = 2.0*B*D
      DD = D*D
      DO  180 I=1,2
         JPD = 5*I
         ZU(I) = A*PD(JPD-4)+C*PD(JPD-3)
         ZV(I) = B*PD(JPD-4)+D*PD(JPD-3)
         ZUU(I) = AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
         ZUV(I) = AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
         ZVV(I) = BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
  180 CONTINUE
C
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
C
      P00 = Z(1)
      P10 = ZU(1)
      P01 = ZV(1)
      P20 = 0.5*ZUU(1)
      P11 = ZUV(1)
      P02 = 0.5*ZVV(1)
      H1 = Z(2)-P00-P01-P02
      H2 = ZV(2)-P01-ZVV(1)
      H3 = ZVV(2)-ZVV(1)
      P03 = 10.0*H1-4.0*H2+0.5*H3
      P04 = -15.0*H1+7.0*H2-H3
      P05 = 6.0*H1-3.0*H2+0.5*H3
      H1 = ZU(2)-P10-P11
      H2 = ZUV(2)-P11
      P12 = 3.0*H1-H2
      P13 = -2.0*H1+H2
      P21 = 0.0
      P23 = -ZUU(2)+ZUU(1)
      P22 = -1.5*P23
      ITPV = IT0
C
C CONVERTS XII AND YII TO U-V SYSTEM.
C
  190 DX = XII-X0
      DY = YII-Y0
      U = AP*DX+BP*DY
      V = CP*DX+DP*DY
C
C EVALUATES THE POLYNOMIAL.
C
      P0 = P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1 = P10+V*(P11+V*(P12+V*P13))
      P2 = P20+V*(P21+V*(P22+V*P23))
      ZII = P0+U*(P1+U*P2)
      RETURN
C
C CALCULATION OF ZII BY EXTRATERPOLATION IN THE TRIANGLE.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
C
  200 IF (IT0 .EQ. ITPV) GO TO  220
C
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE VERTEX
C OF THE TRIANGLE.
C
      JIPL = 3*IL2-2
      IDP = IPL(JIPL)
      X(1) = XD(IDP)
      Y(1) = YD(IDP)
      Z(1) = ZD(IDP)
      JPDD = 5*(IDP-1)
      DO  210 KPD=1,5
         JPDD = JPDD+1
         PD(KPD) = PDD(JPDD)
  210 CONTINUE
C
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
C
      P00 = Z(1)
      P10 = PD(1)
      P01 = PD(2)
      P20 = 0.5*PD(3)
      P11 = PD(4)
      P02 = 0.5*PD(5)
      ITPV = IT0
C
C CONVERTS XII AND YII TO U-V SYSTEM.
C
  220 U = XII-X(1)
      V = YII-Y(1)
C
C EVALUATES THE POLYNOMIAL.
C
      P0 = P00+V*(P01+V*P02)
      P1 = P10+V*P11
      ZII = P0+U*(P1+U*P20)
      RETURN
      END
