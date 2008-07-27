C
C $Id: idptip.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDPTIP (XD,YD,ZD,NT,IPT,NL,IPL,PDD,ITI,XII,YII,ZII)
C
C This subroutine performs punctual interpolation or extrapolation,
C i.e., determines the Z value at a point.
C
C The input arguments are as follows:
C
C   XD,YD,ZD  Arrays of dimension NDP containing the X, Y, and
C             Z coordinates of the data points, where NDP is
C             the number of the data points.
C
C   NT        number of triangles.
C
C   IPT       integer array of dimension 3*NT containing the
C             point numbers of the vertices of the triangles.
C
C   NL        number of border line segments.
C
C   IPL       integer array of dimension 3*NL containing the
C             point numbers of the end points of the border
C             line segments and their respective triangle
C             numbers.
C
C   PDD       array of dimension 5*NDP containing the partial
C             derivatives at the data points.
C
C   ITI       triangle number of the triangle in which lies
C             the point for which interpolation is to be
C             performed.
C
C   XII,YII   X and Y coordinates of the point for which
C             interpolation is to be performed.
C
C The output argument is as follows:
C
C   ZII       interpolated Z value.
C
C Declaration statements.
C
      DIMENSION XD(*),YD(*),ZD(*),IPT(*),IPL(*),PDD(*)
C
      COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
      SAVE   /IDCOMN/
C
      COMMON /IDPT/ ITPV,X0,Y0,AP,BP,CP,DP,
     1              P00,P10,P20,P30,P40,P50,P01,P11,P21,P31,P41,
     2              P02,P12,P22,P32,P03,P13,P23,P04,P14,P05
      SAVE   /IDPT/
C
      DIMENSION   X(3),Y(3),Z(3),PD(15),
     1            ZU(3),ZV(3),ZUU(3),ZUV(3),ZVV(3)
C
      EQUIVALENCE (P5,P50)
C
C Statement function.
C
      VPDT(U1,V1,U2,V2,U3,V3)=(U1-U3)*(V2-V3)-(V1-V3)*(U2-U3)
C
C Preliminary processing.
C
      NTL=NT+NL
      IF (ITI.LE.NTL) GO TO 101
      IL1=ITI/NTL
      IF (INTY.NE.0) GO TO 115
      IL2=ITI-IL1*NTL
      IF (IL1.EQ.IL2) GO TO 107
      GO TO 112
C
C Calculation of ZII by interpolation.  Jump if the necessary
C coefficients have already been calculated.
C
  101 IF (ITI.EQ.ITPV) GO TO 105
C
C Load coordinate and partial-derivative values at the vertices.
C
      JIPT=3*(ITI-1)
      JPD=0
      DO 103 I=1,3
        JIPT=JIPT+1
        IDP=IPT(JIPT)
        X(I)=XD(IDP)
        Y(I)=YD(IDP)
        Z(I)=ZD(IDP)
        IF (INTY.NE.0) GO TO 103
        JPDD=5*(IDP-1)
        DO 102 KPD=1,5
          JPD=JPD+1
          JPDD=JPDD+1
          PD(JPD)=PDD(JPDD)
  102   CONTINUE
  103 CONTINUE
C
C Determine the coefficients for the coordinate system
C transformation from the X-Y system to the U-V system
C and vice-versa.
C
      X0=X(1)
      Y0=Y(1)
      A=X(2)-X0
      B=X(3)-X0
      C=Y(2)-Y0
      D=Y(3)-Y0
      AD=A*D
      BC=B*C
      DLT=AD-BC
      IF (INTY.NE.0) THEN
        AP=(D*(Z(2)-Z(1))-C*(Z(3)-Z(1)))/DLT
        BP=(B*(Z(1)-Z(2))-A*(Z(1)-Z(3)))/DLT
        CP=Z(1)-AP*X(1)-BP*Y(1)
        GO TO 106
      END IF
      AP= D/DLT
      BP=-B/DLT
      CP=-C/DLT
      DP= A/DLT
C
C Convert the partial derivatives at the vertices of the
C triangle for the U-V coordinate system.
C
      AA=A*A
      ACT2=2.0*A*C
      CC=C*C
      AB=A*B
      ADBC=AD+BC
      CD=C*D
      BB=B*B
      BDT2=2.0*B*D
      DD=D*D
      DO 104 I=1,3
        JPD=5*I
        ZU(I)=A*PD(JPD-4)+C*PD(JPD-3)
        ZV(I)=B*PD(JPD-4)+D*PD(JPD-3)
        ZUU(I)=AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
        ZUV(I)=AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
        ZVV(I)=BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
  104 CONTINUE
C
C Calculate the coefficients of the polynomial.
C
      P00=Z(1)
      P10=ZU(1)
      P01=ZV(1)
      P20=0.5*ZUU(1)
      P11=ZUV(1)
      P02=0.5*ZVV(1)
      H1=Z(2)-P00-P10-P20
      H2=ZU(2)-P10-ZUU(1)
      H3=ZUU(2)-ZUU(1)
      P30= 10.0*H1-4.0*H2+0.5*H3
      P40=-15.0*H1+7.0*H2    -H3
      P50=  6.0*H1-3.0*H2+0.5*H3
      H1=Z(3)-P00-P01-P02
      H2=ZV(3)-P01-ZVV(1)
      H3=ZVV(3)-ZVV(1)
      P03= 10.0*H1-4.0*H2+0.5*H3
      P04=-15.0*H1+7.0*H2    -H3
      P05=  6.0*H1-3.0*H2+0.5*H3
      RLU=SQRT(AA+CC)
      RLV=SQRT(BB+DD)
      THXU=ATAN2(C,A)
      THUV=ATAN2(D,B)-THXU
      CSUV=COS(THUV)
      P41=5.0*RLV*CSUV/RLU*P50
      P14=5.0*RLU*CSUV/RLV*P05
      H1=ZV(2)-P01-P11-P41
      H2=ZUV(2)-P11-4.0*P41
      P21= 3.0*H1-H2
      P31=-2.0*H1+H2
      H1=ZU(3)-P10-P11-P14
      H2=ZUV(3)-P11-4.0*P14
      P12= 3.0*H1-H2
      P13=-2.0*H1+H2
      THUS=ATAN2(D-C,B-A)-THXU
      THSV=THUV-THUS
      AA= SIN(THSV)/RLU
      BB=-COS(THSV)/RLU
      CC= SIN(THUS)/RLV
      DD= COS(THUS)/RLV
      AC=AA*CC
      AD=AA*DD
      BC=BB*CC
      G1=AA*AC*(3.0*BC+2.0*AD)
      G2=CC*AC*(3.0*AD+2.0*BC)
      H1=-AA*AA*AA*(5.0*AA*BB*P50+(4.0*BC+AD)*P41)
     1   -CC*CC*CC*(5.0*CC*DD*P05+(4.0*AD+BC)*P14)
      H2=0.5*ZVV(2)-P02-P12
      H3=0.5*ZUU(3)-P20-P21
      P22=(G1*H2+G2*H3-H1)/(G1+G2)
      P32=H2-P22
      P23=H3-P22
      ITPV=ITI
C
C Convert XII and YII to the U-V system.
C
  105 IF (INTY.NE.0) GO TO 106
      DX=XII-X0
      DY=YII-Y0
      U=AP*DX+BP*DY
      V=CP*DX+DP*DY
C
C Evaluate the polynomial.
C
      P0=P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1=P10+V*(P11+V*(P12+V*(P13+V*P14)))
      P2=P20+V*(P21+V*(P22+V*P23))
      P3=P30+V*(P31+V*P32)
      P4=P40+V*P41
      ZII=P0+U*(P1+U*(P2+U*(P3+U*(P4+U*P5))))
C
      RETURN
C
C Linear interpolation.
C
  106 ZII=AP*XII+BP*YII+CP
C
      RETURN
C
C Calculation of ZII by extrapolation in the rectangle.  First,
C check if the necessary coefficients have been calculated.
C
  107 IF (ITI.EQ.ITPV) GO TO 111
C
C Load coordinate and partial-derivative values at the end
C points of the border line segment.
C
      JIPL=3*(IL1-1)
      JPD=0
      DO 109 I=1,2
        JIPL=JIPL+1
        IDP=IPL(JIPL)
        X(I)=XD(IDP)
        Y(I)=YD(IDP)
        Z(I)=ZD(IDP)
        JPDD=5*(IDP-1)
        DO 108 KPD=1,5
          JPD=JPD+1
          JPDD=JPDD+1
          PD(JPD)=PDD(JPDD)
  108   CONTINUE
  109 CONTINUE
C
C Determine the coefficients for the coordinate system
C transformation from the X-Y system to the U-V system
C and vice-versa.
C
      X0=X(1)
      Y0=Y(1)
      A=Y(2)-Y(1)
      B=X(2)-X(1)
      C=-B
      D=A
      AD=A*D
      BC=B*C
      DLT=AD-BC
      AP= D/DLT
      BP=-B/DLT
      CP=-BP
      DP= AP
C
C Convert the partial derivatives at the end points of the
C border line segment for the U-V coordinate system.
C
      AA=A*A
      ACT2=2.0*A*C
      CC=C*C
      AB=A*B
      ADBC=AD+BC
      CD=C*D
      BB=B*B
      BDT2=2.0*B*D
      DD=D*D
      DO 110 I=1,2
        JPD=5*I
        ZU(I)=A*PD(JPD-4)+C*PD(JPD-3)
        ZV(I)=B*PD(JPD-4)+D*PD(JPD-3)
        ZUU(I)=AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
        ZUV(I)=AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
        ZVV(I)=BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
  110 CONTINUE
C
C Calculate the coefficients of the polynomial.
C
      P00=Z(1)
      P10=ZU(1)
      P01=ZV(1)
      P20=0.5*ZUU(1)
      P11=ZUV(1)
      P02=0.5*ZVV(1)
      H1=Z(2)-P00-P01-P02
      H2=ZV(2)-P01-ZVV(1)
      H3=ZVV(2)-ZVV(1)
      P03= 10.0*H1-4.0*H2+0.5*H3
      P04=-15.0*H1+7.0*H2    -H3
      P05=  6.0*H1-3.0*H2+0.5*H3
      H1=ZU(2)-P10-P11
      H2=ZUV(2)-P11
      P12= 3.0*H1-H2
      P13=-2.0*H1+H2
      P21=0.0
      P23=-ZUU(2)+ZUU(1)
      P22=-1.5*P23
      ITPV=ITI
C
C Convert XII and YII to the U-V system.
C
  111 DX=XII-X0
      DY=YII-Y0
      U=AP*DX+BP*DY
      V=CP*DX+DP*DY
C
C Evaluate the polynomial.
C
      P0=P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1=P10+V*(P11+V*(P12+V*P13))
      P2=P20+V*(P21+V*(P22+V*P23))
      ZII=P0+U*(P1+U*P2)
C
      RETURN
C
C Calculate ZII by extrapolation in the triangle.  First,
C check if the necessary coefficients have been calculated.
C
  112 IF (ITI.EQ.ITPV) GO TO 114
C
C Load coordinate and partial-derivative values at the vertex
C of the triangle.
C
      JIPL=3*IL2-2
      IDP=IPL(JIPL)
      X0=XD(IDP)
      Y0=YD(IDP)
      Z0=ZD(IDP)
      JPDD=5*(IDP-1)
      DO 113 KPD=1,5
        JPDD=JPDD+1
        PD(KPD)=PDD(JPDD)
  113 CONTINUE
C
C Calculate the coefficients of the polynomial.
C
      P00=Z0
      P10=PD(1)
      P01=PD(2)
      P20=0.5*PD(3)
      P11=PD(4)
      P02=0.5*PD(5)
      ITPV=ITI
C
C Convert XII and YII to the U-V system.
C
  114 U=XII-X0
      V=YII-Y0
C
C Evaluate the polynomial.
C
      P0=P00+V*(P01+V*P02)
      P1=P10+V*P11
      ZII=P0+U*(P1+U*P20)
C
      RETURN
C
C Linear extrapolation.
C
  115 DO 116 I=1,2
        X(I)=XD(IPL(3*(IL1-1)+I))
        Y(I)=YD(IPL(3*(IL1-1)+I))
        Z(I)=ZD(IPL(3*(IL1-1)+I))
  116 CONTINUE
C
C Use linear interpolation in one of the two triangles formed from the
C the quadrilateral defined by the ends of the edge segment and two
C points that are twice as far from (XAVG,YAVG) as those endpoints and
C have Z values on the least-squares-fit plane.
C
      IF ((X(1)-XAVG)*(X(1)-XAVG)+(Y(1)-YAVG)*(Y(1)-YAVG).LT.
     +    (X(2)-XAVG)*(X(2)-XAVG)+(Y(2)-YAVG)*(Y(2)-YAVG)) THEN
        X(3)=XAVG+2.*(X(1)-XAVG)
        Y(3)=YAVG+2.*(Y(1)-YAVG)
        Z(3)=ALSP*X(3)+BLSP*Y(3)+CLSP
        IF (VPDT(X(2),Y(2),X(3),Y(3),XII,YII).GT.0.) THEN
          X(1)=XAVG+2.*(X(2)-XAVG)
          Y(1)=YAVG+2.*(Y(2)-YAVG)
          Z(1)=ALSP*X(1)+BLSP*Y(1)+CLSP
          IF (VPDT(X(1),Y(1),X(3),Y(3),XII,YII).GT.0.) THEN
            ZII=ALSP*XII+BLSP*YII+CLSP
            RETURN
          END IF
        END IF
      ELSE
        X(3)=XAVG+2.*(X(2)-XAVG)
        Y(3)=YAVG+2.*(Y(2)-YAVG)
        Z(3)=ALSP*X(3)+BLSP*Y(3)+CLSP
        IF (VPDT(X(1),Y(1),X(3),Y(3),XII,YII).LT.0.) THEN
          X(2)=XAVG+2.*(X(1)-XAVG)
          Y(2)=YAVG+2.*(Y(1)-YAVG)
          Z(2)=ALSP*X(2)+BLSP*Y(2)+CLSP
          IF (VPDT(X(2),Y(2),X(3),Y(3),XII,YII).LT.0.) THEN
            ZII=ALSP*XII+BLSP*YII+CLSP
            RETURN
          END IF
        END IF
      END IF
C
      A=X(2)-X(1)
      B=X(3)-X(1)
      C=Y(2)-Y(1)
      D=Y(3)-Y(1)
      AD=A*D
      BC=B*C
      DLT=AD-BC
      AP=(D*(Z(2)-Z(1))-C*(Z(3)-Z(1)))/DLT
      BP=(B*(Z(1)-Z(2))-A*(Z(1)-Z(3)))/DLT
      CP=Z(1)-AP*X(1)-BP*Y(1)
      ZII=AP*XII+BP*YII+CP
C
      RETURN
C
      END
