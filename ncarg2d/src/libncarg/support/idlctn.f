C
C $Id: idlctn.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDLCTN (NDP,XD,YD,NT,IPT,NL,IPL,XII,YII,ITI,IWK,WK)
C
C This subroutine locates a point, i.e., it determines to what triangle
C a given point (XII,YII) belongs.  If the given point does not lie
C inside the data area and quintic interpolation is selected, IDLCTN
C determines which border line segment defines the semi-infinite
C rectangular area containing the point or, alternatively, which pair
C of border line segments defines the semi-infinite triangular area
C containing the point.  If the point lies outside the data area but
C linear interpolation is selected, IDLCTN determines which border line
C segment defines the semi-infinite quadrangular area containing the
C point.
C
C The input arguments are as follows:
C
C   NDP      number of data points.
C
C   XD,YD    arrays of dimension NDP containing the X and Y
C            coordinates of the data points.
C
C   NT       number of triangles.
C
C   IPT      integer array of dimension 3*NT containing the
C            point numbers of the vertices of the triangles.
C
C   NL       number of border line segments.
C
C   IPL      integer array of dimension 3*NL containing the
C            point numbers of the end points of the border
C            line segments and their respective triangle
C            numbers.
C
C   XII,YII  X and Y coordinates of the point to be located.
C
C The output argument is as follows:
C
C   ITI      triangle number, when the point is inside the
C            data area, or two border line segment numbers,
C            IL1 and IL2, coded to IL1*(NT+NL)+IL2, when the
C            point is outside the data area.
C
C The other arguments are as follows:
C
C   IWK      integer array of dimension 18*NDP used internally
C            as a work area.
C
C   WK       array of dimension 8*NDP used internally as a
C            work area.
C
C Declaration statements.

      DIMENSION XD(NDP),YD(NDP),IPT(3*NT),IPL(3*NL),IWK(18*NDP),
     +          WK(8*NDP),IDSC(9)
C
      COMMON /IDLC/ ITIPV,XS1,XS2,YS1,YS2,NTSC(9)
      SAVE   /IDLC/
C
      COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
      SAVE   /IDCOMN/
C
C Statement functions.
C
      SPDT(U1,V1,U2,V2,U3,V3)=(U1-U2)*(U3-U2)+(V1-V2)*(V3-V2)
      VPDT(U1,V1,U2,V2,U3,V3)=(U1-U3)*(V2-V3)-(V1-V3)*(U2-U3)
C
C Preliminary processing.
C
      NTL=NT+NL
      X0=XII
      Y0=YII
C
C Jump if processing the same set of data as in the previous call.
C
      IF (ITIPV.NE.0) GO TO 108
C
C Divide the X-Y plane into nine rectangular sections.
C
      XMN=XD(1)
      XMX=XMN
      YMN=YD(1)
      YMX=YMN
C
      DO 101 IDP=2,NDP
        XMN=MIN(XD(IDP),XMN)
        XMX=MAX(XD(IDP),XMX)
        YMN=MIN(YD(IDP),YMN)
        YMX=MAX(YD(IDP),YMX)
  101 CONTINUE
C
      XS1=(XMN+XMN+XMX)/3.0
      XS2=(XMN+XMX+XMX)/3.0
      YS1=(YMN+YMN+YMX)/3.0
      YS2=(YMN+YMX+YMX)/3.0
C
C Determine and store in the array IWK triangle numbers of the triangles
C associated with each of the nine sections and in the array WK the
C minimum and maximum of the X and Y coordinate values for each of the
C triangles.
C
      DO 102 ISC=1,9
        NTSC(ISC)=0
        IDSC(ISC)=0
  102 CONTINUE
C
      IT0T3=0
      JWK=0
C
      DO 107 IT0=1,NT
C
        IT0T3=IT0T3+3
        I1=IPT(IT0T3-2)
        I2=IPT(IT0T3-1)
        I3=IPT(IT0T3)
        XMN=MIN(XD(I1),XD(I2),XD(I3))
        XMX=MAX(XD(I1),XD(I2),XD(I3))
        YMN=MIN(YD(I1),YD(I2),YD(I3))
        YMX=MAX(YD(I1),YD(I2),YD(I3))
        IF (YMN.GT.YS1)                   GO TO 103
        IF (XMN.LE.XS1)                   IDSC(1)=1
        IF (XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(2)=1
        IF (XMX.GE.XS2)                   IDSC(3)=1
  103   IF (YMX.LT.YS1.OR.YMN.GT.YS2)     GO TO 104
        IF (XMN.LE.XS1)                   IDSC(4)=1
        IF (XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(5)=1
        IF (XMX.GE.XS2)                   IDSC(6)=1
  104   IF (YMX.LT.YS2)                   GO TO 105
        IF (XMN.LE.XS1)                   IDSC(7)=1
        IF (XMX.GE.XS1.AND.XMN.LE.XS2)    IDSC(8)=1
        IF (XMX.GE.XS2)                   IDSC(9)=1
C
  105   DO 106 ISC=1,9
          IF (IDSC(ISC).EQ.0) GO TO 106
          JIWK=9*NTSC(ISC)+ISC
          IWK(JIWK)=IT0
          NTSC(ISC)=NTSC(ISC)+1
          IDSC(ISC)=0
  106   CONTINUE
C
        JWK=JWK+4
C
        WK(JWK-3)=XMN
        WK(JWK-2)=XMX
        WK(JWK-1)=YMN
        WK(JWK)  =YMX
C
  107 CONTINUE
C
      GO TO 111
C
C Check if in the same triangle as previously.
C
  108 IT0=ITIPV
      IF (IT0.GT.NT) GO TO 109
      IT0T3=IT0*3
      IP1=IPT(IT0T3-2)
      X1=XD(IP1)
      Y1=YD(IP1)
      IP2=IPT(IT0T3-1)
      X2=XD(IP2)
      Y2=YD(IP2)
      IF (VPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.) GO TO 111
      IP3=IPT(IT0T3)
      X3=XD(IP3)
      Y3=YD(IP3)
      IF (VPDT(X2,Y2,X3,Y3,X0,Y0).LT.0.) GO TO 111
      IF (VPDT(X3,Y3,X1,Y1,X0,Y0).LT.0.) GO TO 111
      GO TO 117
C
C Check if on the same border line segment.
C
  109 IL1=IT0/NTL
      IL2=IT0-IL1*NTL
      IL1T3=IL1*3
      IP1=IPL(IL1T3-2)
      X1=XD(IP1)
      Y1=YD(IP1)
      IP2=IPL(IL1T3-1)
      X2=XD(IP2)
      Y2=YD(IP2)
C
      IF (IL2.NE.IL1) GO TO 110
C
      IF (VPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.) GO TO 111
C
      IF (INTY.EQ.0) THEN
        IF (SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.) GO TO 111
        IF (SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.) GO TO 111
      ELSE
        IF (VPDT(XAVG,YAVG,X1,Y1,X0,Y0).LT.0.) GO TO 111
        IF (VPDT(XAVG,YAVG,X2,Y2,X0,Y0).GT.0.) GO TO 111
      END IF
C
      GO TO 117
C
C Check if between the same two border line segments.
C
  110 IF (SPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.) GO TO 111
      IP3=IPL(3*IL2-1)
      X3=XD(IP3)
      Y3=YD(IP3)
      IF (SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.) GO TO 117
C
C Locate inside the data area.  Determine the section in which
C the point in question lies.
C
  111 ISC=1
      IF (X0.GE.XS1) ISC=ISC+1
      IF (X0.GE.XS2) ISC=ISC+1
      IF (Y0.GE.YS1) ISC=ISC+3
      IF (Y0.GE.YS2) ISC=ISC+3
C
C Search through the triangles associated with the section.
C
      NTSCI=NTSC(ISC)
      IF (NTSCI.LE.0) GO TO 113
C
      JIWK=-9+ISC
C
      DO 112 ITSC=1,NTSCI
        JIWK=JIWK+9
        IT0=IWK(JIWK)
        JWK=IT0*4
        IF (X0.LT.WK(JWK-3)) GO TO 112
        IF (X0.GT.WK(JWK-2)) GO TO 112
        IF (Y0.LT.WK(JWK-1)) GO TO 112
        IF (Y0.GT.WK(JWK))   GO TO 112
        IT0T3=IT0*3
        IP1=IPT(IT0T3-2)
        X1=XD(IP1)
        Y1=YD(IP1)
        IP2=IPT(IT0T3-1)
        X2=XD(IP2)
        Y2=YD(IP2)
        IF (VPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.) GO TO 112
        IP3=IPT(IT0T3)
        X3=XD(IP3)
        Y3=YD(IP3)
        IF (VPDT(X2,Y2,X3,Y3,X0,Y0).LT.0.) GO TO 112
        IF (VPDT(X3,Y3,X1,Y1,X0,Y0).LT.0.) GO TO 112
        GO TO 117
  112 CONTINUE
C
C Locate outside the data area.
C
  113 DO 115 IL1=1,NL
        IL1T3=IL1*3
        IP1=IPL(IL1T3-2)
        X1=XD(IP1)
        Y1=YD(IP1)
        IP2=IPL(IL1T3-1)
        X2=XD(IP2)
        Y2=YD(IP2)
        IF (INTY.EQ.0) THEN
          IF (SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.) GO TO 115
          IF (SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.) GO TO 114
          IF (VPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.) GO TO 115
          IL2=IL1
          GO TO 116
  114     IL2=MOD(IL1,NL)+1
          IP3=IPL(3*IL2-1)
          X3=XD(IP3)
          Y3=YD(IP3)
          IF (SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.) GO TO 116
        ELSE
          IF (VPDT(X1  ,Y1  ,X2,Y2,X0,Y0).GT.0.) GO TO 115
          IF (VPDT(XAVG,YAVG,X1,Y1,X0,Y0).LT.0.) GO TO 115
          IF (VPDT(XAVG,YAVG,X2,Y2,X0,Y0).GT.0.) GO TO 115
          IL2=IL1
          GO TO 116
        END IF
  115 CONTINUE
C
      IT0=1
      GO TO 117
C
  116 IT0=IL1*NTL+IL2
C
C Normal exit.
C
  117 ITI=IT0
      ITIPV=IT0
C
      RETURN
C
      END
