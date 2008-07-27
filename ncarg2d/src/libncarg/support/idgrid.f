C
C $Id: idgrid.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDGRID (XD,YD,NT,IPT,NL,IPL,NXI,NYI,XI,YI,NGP,IGP)
C
C This subroutine organizes grid points for surface fitting by
C sorting them in ascending order of triangle number or border
C line segment number.
C
C The input arguments are as follows:
C
C   XD,YD  arrays of dimension NDP containing the X and Y
C          coordinates of the data points, where NDP is the
C          number of the data points.
C
C   NT     number of triangles.
C
C   IPT    integer array of dimension 3*NT containing the
C          point numbers of the vertices of the triangles.
C
C   NL     number of border line segments.
C
C   IPL    integer array of dimension 3*NL containing the
C          point numbers of the end points of the border
C          line segments and their respective triangle
C          numbers.
C
C   NXI    number of grid points in the X coordinate.
C
C   NYI    number of grid points in the Y coordinate.
C
C   XI,YI  arrays of dimension NXI and NYI containing
C          the X and Y coordinates of the grid points,
C          respectively.
C
C The output arguments are as follows:
C
C   NGP    integer array of dimension 2*(NT+2*NL) where the
C          number of grid points that belong to each of the
C          triangles or of the border line segments are to
C          be stored.
C
C   IGP    integer array of dimension NXI*NYI where the
C          grid point numbers are to be stored in ascending
C          order of the triangle number or border line
C          segment number.
C
C Declaration statements:
C
      DIMENSION XD(*),YD(*),IPT(*),IPL(*),XI(*),YI(*),NGP(*),IGP(*)
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
      NXINYI=NXI*NYI
C
      XIMN=MIN(XI(1),XI(NXI))
      XIMX=MAX(XI(1),XI(NXI))
      YIMN=MIN(YI(1),YI(NYI))
      YIMX=MAX(YI(1),YI(NYI))
C
C Determine grid points inside the data area.
C
      JNGP0=0
      JNGP1=2*(NT+2*NL)+1
C
      JIGP0=0
      JIGP1=NXINYI+1
C
      DO 114 IT0=1,NT
C
        NGP0=0
        NGP1=0
C
        X1=XD(IPT(IT0*3-2))
        Y1=YD(IPT(IT0*3-2))
        X2=XD(IPT(IT0*3-1))
        Y2=YD(IPT(IT0*3-1))
        X3=XD(IPT(IT0*3))
        Y3=YD(IPT(IT0*3))
C
        XMN=MIN(X1,X2,X3)
        XMX=MAX(X1,X2,X3)
        YMN=MIN(Y1,Y2,Y3)
        YMX=MAX(Y1,Y2,Y3)
C
        INSD=0
C
        DO 102 IXI=1,NXI
          IF (XI(IXI).GE.XMN.AND.XI(IXI).LE.XMX) GO TO 101
          IF (INSD.EQ.0) GO TO 102
          IXIMX=IXI-1
          GO TO 103
  101     IF (INSD.EQ.1) GO TO 102
          INSD=1
          IXIMN=IXI
  102   CONTINUE
C
        IF (INSD.EQ.0) GO TO 113
C
        IXIMX=NXI
C
  103   DO 112 IYI=1,NYI
C
          YII=YI(IYI)
C
          IF (YII.LT.YMN.OR.YII.GT.YMX) GO TO 112
C
          DO 111  IXI=IXIMN,IXIMX
C
            XII=XI(IXI)
C
            L=0
            IF (VPDT(X1,Y1,X2,Y2,XII,YII)) 111,104,105
  104       L=1
  105       IF (VPDT(X2,Y2,X3,Y3,XII,YII)) 111,106,107
  106       L=1
  107       IF (VPDT(X3,Y3,X1,Y1,XII,YII)) 111,108,109
  108       L=1
  109       IZI=NXI*(IYI-1)+IXI
C
            IF (L.EQ.0) THEN
              NGP0=NGP0+1
              JIGP0=JIGP0+1
              IF (JIGP0.GE.JIGP1) THEN
                CALL SETER ('IDGRID (BIVAR) - INTERNAL ERROR - SEE CONSU
     +LTANT',1,1)
                RETURN
              END IF
              IGP(JIGP0)=IZI
            ELSE
              IF (JIGP1.LE.NXINYI) THEN
                DO 110 JIGP1I=JIGP1,NXINYI
                  IF (IZI.EQ.IGP(JIGP1I)) GO TO 111
  110           CONTINUE
              END IF
              NGP1=NGP1+1
              JIGP1=JIGP1-1
              IF (JIGP1.LE.JIGP0) THEN
                CALL SETER ('IDGRID (BIVAR) - INTERNAL ERROR - SEE CONSU
     +LTANT',2,1)
                RETURN
              END IF
              IGP(JIGP1)=IZI
            END IF
C
  111     CONTINUE
C
  112   CONTINUE
C
  113   JNGP0=JNGP0+1
        NGP(JNGP0)=NGP0
C
        JNGP1=JNGP1-1
        NGP(JNGP1)=NGP1
C
  114 CONTINUE
C
C If linear interpolation is selected, jump to do the semi-infinite
C quadrangular areas formed by rays emanating from (XAVG,YAVG).
C
      IF (INTY.NE.0) GO TO 300
C
C Determine grid points outside the data area in each of the
C semi-infinite rectangular and triangular areas.
C
      DO 263 IL0=1,NL
C
C Rectangular area ...
C
        NGP0=0
        NGP1=0
C
        X1=XD(IPL(IL0*3-2))
        Y1=YD(IPL(IL0*3-2))
        X2=XD(IPL(IL0*3-1))
        Y2=YD(IPL(IL0*3-1))
C
        XMN=XIMN
        XMX=XIMX
        YMN=YIMN
        YMX=YIMX
C
        IF (Y2.GE.Y1) XMN=MIN(X1,X2)
        IF (Y2.LE.Y1) XMX=MAX(X1,X2)
        IF (X2.LE.X1) YMN=MIN(Y1,Y2)
        IF (X2.GE.X1) YMX=MAX(Y1,Y2)
C
        INSD=0
C
        DO 202 IXI=1,NXI
          IF (XI(IXI).GE.XMN.AND.XI(IXI).LE.XMX) GO TO 201
          IF (INSD.EQ.0) GO TO 202
          IXIMX=IXI-1
          GO TO 203
  201     IF (INSD.EQ.1) GO TO 202
          INSD=1
          IXIMN=IXI
  202   CONTINUE
C
        IF (INSD.EQ.0) GO TO 213
C
        IXIMX=NXI
C
  203   DO 212 IYI=1,NYI
C
          YII=YI(IYI)
C
          IF (YII.LT.YMN.OR.YII.GT.YMX) GO TO 212
C
          DO 211 IXI=IXIMN,IXIMX
C
            XII=XI(IXI)
C
            L=0
            IF (VPDT(X1,Y1,X2,Y2,XII,YII)) 205,204,211
  204       L=1
  205       IF (SPDT(X2,Y2,X1,Y1,XII,YII)) 211,206,207
  206       L=1
  207       IF (SPDT(X1,Y1,X2,Y2,XII,YII)) 211,208,209
  208       L=1
  209       IZI=NXI*(IYI-1)+IXI
C
            IF (L.EQ.0) THEN
              NGP0=NGP0+1
              JIGP0=JIGP0+1
              IF (JIGP0.GE.JIGP1) THEN
                CALL SETER ('IDGRID (BIVAR) - INTERNAL ERROR - SEE CONSU
     +LTANT',3,1)
                RETURN
              END IF
              IGP(JIGP0)=IZI
            ELSE
              IF (JIGP1.LE.NXINYI) THEN
                DO 210 JIGP1I=JIGP1,NXINYI
                  IF (IZI.EQ.IGP(JIGP1I)) GO TO 211
  210           CONTINUE
              END IF
              NGP1=NGP1+1
              JIGP1=JIGP1-1
              IF (JIGP1.LE.JIGP0) THEN
                CALL SETER ('IDGRID (BIVAR) - INTERNAL ERROR - SEE CONSU
     +LTANT',4,1)
                RETURN
              END IF
              IGP(JIGP1)=IZI
            END IF
C
  211     CONTINUE
C
  212   CONTINUE
C
  213   JNGP0=JNGP0+1
        NGP(JNGP0)=NGP0
C
        JNGP1=JNGP1-1
        NGP(JNGP1)=NGP1
C
C Triangular area ...
C
        NGP0=0
        NGP1=0
C
        ILP1=MOD(IL0,NL)+1
C
        X3=XD(IPL(ILP1*3-1))
        Y3=YD(IPL(ILP1*3-1))
C
        XMN=XIMN
        XMX=XIMX
        YMN=YIMN
        YMX=YIMX
C
        IF (Y3.GE.Y2.AND.Y2.GE.Y1) XMN=X2
        IF (Y3.LE.Y2.AND.Y2.LE.Y1) XMX=X2
        IF (X3.LE.X2.AND.X2.LE.X1) YMN=Y2
        IF (X3.GE.X2.AND.X2.GE.X1) YMX=Y2
C
        INSD=0
C
        DO 252 IXI=1,NXI
          IF (XI(IXI).GE.XMN.AND.XI(IXI).LE.XMX) GO TO 251
          IF (INSD.EQ.0) GO TO 252
          IXIMX=IXI-1
          GO TO 253
  251     IF (INSD.EQ.1) GO TO 252
          INSD=1
          IXIMN=IXI
  252   CONTINUE
C
        IF (INSD.EQ.0) GO TO 261
C
        IXIMX=NXI
C
  253   DO 260 IYI=1,NYI
C
          YII=YI(IYI)
C
          IF (YII.LT.YMN.OR.YII.GT.YMX) GO TO 260
C
          DO 259 IXI=IXIMN,IXIMX
C
            XII=XI(IXI)
C
            L=0
            IF (SPDT(X1,Y1,X2,Y2,XII,YII)) 255,254,259
  254       L=1
  255       IF (SPDT(X3,Y3,X2,Y2,XII,YII)) 257,256,259
  256       L=1
  257       IZI=NXI*(IYI-1)+IXI
C
            IF (L.EQ.0) THEN
              NGP0=NGP0+1
              JIGP0=JIGP0+1
              IF (JIGP0.GE.JIGP1) THEN
                CALL SETER ('IDGRID (BIVAR) - INTERNAL ERROR - SEE CONSU
     +LTANT',5,1)
                RETURN
              END IF
              IGP(JIGP0)=IZI
            ELSE
              IF (JIGP1.LE.NXINYI) THEN
                DO 258 JIGP1I=JIGP1,NXINYI
                  IF (IZI.EQ.IGP(JIGP1I)) GO TO 259
  258           CONTINUE
              END IF
              NGP1=NGP1+1
              JIGP1=JIGP1-1
              IF (JIGP1.LE.JIGP0) THEN
                CALL SETER ('IDGRID (BIVAR) - INTERNAL ERROR - SEE CONSU
     +LTANT',6,1)
                RETURN
              END IF
              IGP(JIGP1)=IZI
            END IF
C
  259     CONTINUE
C
  260   CONTINUE
C
  261   JNGP0=JNGP0+1
        NGP(JNGP0)=NGP0
C
        JNGP1=JNGP1-1
        NGP(JNGP1)=NGP1
C
  263 CONTINUE
C
      RETURN
C
C Linear interpolation was selected.  Examine semi-infinite quadrangular
C areas formed by rays emanating from (XAVG,YAVG) and passing through
C the endpoints of a line segment on the edge of the data area.
C
  300 DO 314 IL0=1,NL
C
        NGP0=0
        NGP1=0
C
        X1=XD(IPL(IL0*3-2))
        Y1=YD(IPL(IL0*3-2))
        X2=XD(IPL(IL0*3-1))
        Y2=YD(IPL(IL0*3-1))
C
        XMN=XIMN
        XMX=XIMX
        YMN=YIMN
        YMX=YIMX
C
        IF (XAVG.LE.X1.AND.XAVG.LE.X2) XMN=MIN(X1,X2)
        IF (XAVG.GE.X1.AND.XAVG.GE.X2) XMX=MAX(X1,X2)
        IF (YAVG.LE.Y1.AND.YAVG.LE.Y2) YMN=MIN(Y1,Y2)
        IF (YAVG.GE.Y1.AND.YAVG.GE.Y2) YMX=MAX(Y1,Y2)
C
        INSD=0
C
        DO 302 IXI=1,NXI
          IF (XI(IXI).GE.XMN.AND.XI(IXI).LE.XMX) GO TO 301
          IF (INSD.EQ.0) GO TO 302
          IXIMX=IXI-1
          GO TO 303
  301     IF (INSD.EQ.1) GO TO 302
          INSD=1
          IXIMN=IXI
  302   CONTINUE
C
        IF (INSD.EQ.0) GO TO 313
C
        IXIMX=NXI
C
  303   DO 312 IYI=1,NYI
C
          YII=YI(IYI)
C
          IF (YII.LT.YMN.OR.YII.GT.YMX) GO TO 312
C
          DO 311 IXI=IXIMN,IXIMX
C
            XII=XI(IXI)
C
            L=0
            IF (VPDT(X1  ,Y1  ,X2,Y2,XII,YII)) 305,304,311
  304       L=1
  305       IF (VPDT(XAVG,YAVG,X1,Y1,XII,YII)) 311,306,307
  306       L=1
  307       IF (VPDT(XAVG,YAVG,X2,Y2,XII,YII)) 309,308,311
  308       L=1
  309       IZI=NXI*(IYI-1)+IXI
C
            IF (L.EQ.0) THEN
              NGP0=NGP0+1
              JIGP0=JIGP0+1
              IGP(JIGP0)=IZI
            ELSE
              IF (JIGP1.LE.NXINYI) THEN
                DO 310 JIGP1I=JIGP1,NXINYI
                  IF (IZI.EQ.IGP(JIGP1I)) GO TO 311
  310           CONTINUE
              END IF
              NGP1=NGP1+1
              JIGP1=JIGP1-1
              IGP(JIGP1)=IZI
            END IF
C
  311     CONTINUE
C
  312   CONTINUE
C
  313   JNGP0=JNGP0+1
        NGP(JNGP0)=NGP0
C
        JNGP1=JNGP1-1
        NGP(JNGP1)=NGP1
C
        JNGP0=JNGP0+1
        NGP(JNGP0)=0
C
        JNGP1=JNGP1-1
        NGP(JNGP1)=0
C
  314 CONTINUE
C
      RETURN
C
      END
