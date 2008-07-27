C
C $Id: idtang.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDTANG (NDP,XD,YD,NT,IPT,NL,IPL,IWL,IWP,WK)
C
C This subroutine performs triangulation.  It divides the X-Y
C plane into a number of triangles according to given data
C points in the plane, determines line segments that form the
C border of the data area, and determines the triangle numbers
C corresponding to the border line segments.
C
C At completion, point numbers of the vertices of each triangle
C are listed counter-clockwise.  Point numbers of the end points
C of each border line segment are listed counter-clockwise,
C listing order of the line segments being counter-clockwise.
C
C This subroutine calls the function IDXCHG.
C
C The input arguments are as follows:
C
C   NDP  number of data points.
C
C   XD   array of dimension NDP containing the
C        X coordinates of the data points.
C
C   YD   array of dimension NDP containing the
C        Y coordinates of the data points.
C
C The output arguments are as follows:
C
C   NT    number of triangles.
C
C   IPT  integer array of dimension 6*NDP-15, where the
C        point numbers of the vertices of the (IT)th
C        triangle are to be stored as the (3*IT-2)nd,
C        (3*IT-1)st, and (3*IT)th elements,
C        IT=1,2,...,NT.
C
C   NL   number of border line segments,
C
C   IPL  integer array of dimension 6*NDP, where the
C        point numbers of the end points of the (IL)th
C        border line segment and its respective triangle
C        number are to be stored as the (3*IL-2)nd,
C        (3*IL-1)st, and (3*IL)th elements,
C        IL=1,2,..., NL.
C
C The other arguments are as follows:
C
C   IWL  integer array of dimension 18*NDP used internally as a
C        work area.
C
C   IWP  integer array of dimension NDP used internally as a
C         work area.
C
C   WK   real array of dimension NDP used internally as a
C        work area.
C
C Declaration statements.
C
      DIMENSION XD(NDP),YD(NDP),IPT(6*NDP-15),IPL(6*NDP),IWL(18*NDP),
     +          IWP(NDP),WK(NDP),ITF(2)
C
      DATA EPSLN / 1.E-6 /, NREP / 200 /
C
C Statement functions.
C
      DSQF(U1,V1,U2,V2)=(U2-U1)**2+(V2-V1)**2
      SPDT(U1,V1,U2,V2,U3,V3)=(U2-U1)*(U3-U1)+(V2-V1)*(V3-V1)
      VPDT(U1,V1,U2,V2,U3,V3)=(V3-V1)*(U2-U1)-(U3-U1)*(V2-V1)
C
C Preliminary processing.
C
      IF (NDP.LT.4) THEN
        CALL SETER ('IDTANG (BIVAR) - INPUT PARAMETER NDP OUT OF RANGE',
     +1,1)
        RETURN
      END IF
C
      NDPM1=NDP-1
C
C Determine the closest pair of data points and the midpoint.
C
      DSQMN=DSQF(XD(1),YD(1),XD(2),YD(2))
      IPMN1=1
      IPMN2=2
      DO 102 IP1=1,NDPM1
        X1=XD(IP1)
        Y1=YD(IP1)
        IP1P1=IP1+1
        DO 101 IP2=IP1P1,NDP
          DSQI=DSQF(X1,Y1,XD(IP2),YD(IP2))
          IF (DSQI.EQ.0.) THEN
            CALL SETER ('IDTANG (BIVAR) - TWO OF THE INPUT DATA POINTS A
     +RE IDENTICAL',2,1)
            RETURN
          END IF
          IF (DSQI.GE.DSQMN) GO TO 101
          DSQMN=DSQI
          IPMN1=IP1
          IPMN2=IP2
  101   CONTINUE
  102 CONTINUE
      XDMP=(XD(IPMN1)+XD(IPMN2))/2.0
      YDMP=(YD(IPMN1)+YD(IPMN2))/2.0
C
C Sort the other (NDP-2) data points in ascending order of
C distance from the midpoint and store the sorted data point
C numbers in the IWP array.
C
      JP1=2
C
      DO 103 IP1=1,NDP
        IF (IP1.EQ.IPMN1.OR.IP1.EQ.IPMN2) GO TO 103
        JP1=JP1+1
        IWP(JP1)=IP1
        WK(JP1)=DSQF(XDMP,YDMP,XD(IP1),YD(IP1))
  103 CONTINUE
C
      DO 105 JP1=3,NDPM1
        DSQMN=WK(JP1)
        JPMN=JP1
        DO 104 JP2=JP1,NDP
          IF (WK(JP2).GE.DSQMN) GO TO 104
          DSQMN=WK(JP2)
          JPMN=JP2
  104   CONTINUE
        ITS=IWP(JP1)
        IWP(JP1)=IWP(JPMN)
        IWP(JPMN)=ITS
        WK(JPMN)=WK(JP1)
  105 CONTINUE
C
C If necessary, modify the ordering in such a way that the
C first three data points are not collinear.
C
      X1=XD(IPMN1)
      Y1=YD(IPMN1)
      X2=XD(IPMN2)
      Y2=YD(IPMN2)
C
      DO 106 JP=3,NDP
        IP=IWP(JP)
        SP=SPDT(XD(IP),YD(IP),X1,Y1,X2,Y2)
        VP=VPDT(XD(IP),YD(IP),X1,Y1,X2,Y2)
        IF (ABS(VP).GT.(ABS(SP)*EPSLN)) GO TO 107
  106 CONTINUE
C
      CALL SETER ('IDTANG (BIVAR) - ALL COLLINEAR DATA POINTS',3,1)
      RETURN
C
  107 IF (JP.EQ.3) GO TO 109
      JPMX=JP
C
      DO 108 JPC=4,JPMX
        JP=JPMX+4-JPC
        IWP(JP)=IWP(JP-1)
  108 CONTINUE
C
      IWP(3)=IP
C
C Form the first triangle.  Store point numbers of the vertices
C of the triangle in the IPT array, and store point numbers of
C the border line segments and the triangle number in the IPL
C array.
C
  109 IP1=IPMN1
      IP2=IPMN2
      IP3=IWP(3)
      IF (VPDT(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))
     +                                        .GE.0.) GO TO 110
      IP1=IPMN2
      IP2=IPMN1
  110 NT0=1
      NTT3=3
      IPT(1)=IP1
      IPT(2)=IP2
      IPT(3)=IP3
      NL0=3
      NLT3=9
      IPL(1)=IP1
      IPL(2)=IP2
      IPL(3)=1
      IPL(4)=IP2
      IPL(5)=IP3
      IPL(6)=1
      IPL(7)=IP3
      IPL(8)=IP1
      IPL(9)=1
C
C Add the remaining (NDP-3) data points, one by one.
C
      DO 130 JP1=4,NDP
C
        IP1=IWP(JP1)
        X1=XD(IP1)
        Y1=YD(IP1)
C
C Determine the first invisible and visible border line segments,
C ILIV and ILVS.
C
        DO 113 IL=1,NL0
          IP2=IPL(3*IL-2)
          IP3=IPL(3*IL-1)
          X2=XD(IP2)
          Y2=YD(IP2)
          X3=XD(IP3)
          Y3=YD(IP3)
          SP=SPDT(X1,Y1,X2,Y2,X3,Y3)
          VP=VPDT(X1,Y1,X2,Y2,X3,Y3)
          IF (IL.NE.1) GO TO 111
          IXVS=0
          IF (VP.LE.(ABS(SP)*(-EPSLN))) IXVS=1
          ILIV=1
          ILVS=1
          GO TO 113
  111     IXVSPV=IXVS
          IF (VP.GT.(ABS(SP)*(-EPSLN))) GO TO 112
          IXVS=1
          IF (IXVSPV.EQ.1) GO TO 113
          ILVS=IL
          IF (ILIV.NE.1) GO TO 114
          GO TO 113
  112     IXVS=0
          IF (IXVSPV.EQ.0) GO TO 113
          ILIV=IL
          IF (ILVS.NE.1) GO TO 114
  113   CONTINUE
C
        IF (ILIV.EQ.1.AND.ILVS.EQ.1) ILVS=NL0
  114   IF (ILVS.LT.ILIV) ILVS=ILVS+NL0
C
C Shifts (rotates) the IPL array to have the invisible border
C line segments contained in the first part of the IPL array.
C
        IF (ILIV.EQ.1) GO TO 117
        NLSH=ILIV-1
        NLSHT3=NLSH*3
C
        DO 115 JL1=1,NLSHT3
          JL2=JL1+NLT3
          IPL(JL2)=IPL(JL1)
  115   CONTINUE
C
        DO 116 JL1=1,NLT3
          JL2=JL1+NLSHT3
          IPL(JL1)=IPL(JL2)
  116   CONTINUE
C
        ILVS=ILVS-NLSH
C
C Add triangles to the IPT array, update border line segments
C in the IPL array, and set flags for the border lines segments
C to be re-examined in the IWL array.
C
  117   JWL=0
C
        DO 121 IL=ILVS,NL0
C
          ILT3=IL*3
          IPL1=IPL(ILT3-2)
          IPL2=IPL(ILT3-1)
          IT  =IPL(ILT3)
C
C Add a triangle to the IPT array.
C
          NT0=NT0+1
          NTT3=NTT3+3
          IPT(NTT3-2)=IPL2
          IPT(NTT3-1)=IPL1
          IPT(NTT3)  =IP1
C
C Update border line segments in the IPL array.
C
          IF (IL.NE.ILVS) GO TO 118
          IPL(ILT3-1)=IP1
          IPL(ILT3)  =NT0
  118     IF (IL.NE.NL0) GO TO 119
          NLN=ILVS+1
          NLNT3=NLN*3
          IPL(NLNT3-2)=IP1
          IPL(NLNT3-1)=IPL(1)
          IPL(NLNT3)  =NT0
C
C Determine the vertex that does not lie on the border
C line segments.
C
  119     ITT3=IT*3
          IPTI=IPT(ITT3-2)
          IF (IPTI.NE.IPL1.AND.IPTI.NE.IPL2) GO TO 120
          IPTI=IPT(ITT3-1)
          IF (IPTI.NE.IPL1.AND.IPTI.NE.IPL2) GO TO 120
          IPTI=IPT(ITT3)
C
C Check if an exchange is necessary.
C
  120     IF (IDXCHG(XD,YD,IP1,IPTI,IPL1,IPL2).EQ.0) GO TO 121
C
C Modify the IPT array when necessary.
C
          IPT(ITT3-2)=IPTI
          IPT(ITT3-1)=IPL1
          IPT(ITT3)  =IP1
          IPT(NTT3-1)=IPTI
          IF (IL.EQ.ILVS)  IPL(ILT3)=IT
          IF (IL.EQ.NL0.AND.IPL(3).EQ.IT) IPL(3)=NT0
C
C Set flags in the IWL array.
C
          JWL=JWL+4
          IWL(JWL-3)=IPL1
          IWL(JWL-2)=IPTI
          IWL(JWL-1)=IPTI
          IWL(JWL)  =IPL2
C
  121   CONTINUE
C
        NL0=NLN
        NLT3=NLNT3
        NLF=JWL/2
        IF (NLF.EQ.0) GO TO 130
C
C Improve triangulation.
C
        NTT3P3=NTT3+3
C
        DO 129 IREP=1,NREP
C
          DO 127 ILF=1,NLF
C
            IPL1=IWL(2*ILF-1)
            IPL2=IWL(2*ILF)
C
C Locate in the IPT array two triangles, one on each side of
C the flagged line segment.
C
            NTF=0
C
            DO 122 ITT3R=3,NTT3,3
              ITT3=NTT3P3-ITT3R
              IPT1=IPT(ITT3-2)
              IPT2=IPT(ITT3-1)
              IPT3=IPT(ITT3)
              IF (IPL1.NE.IPT1.AND.IPL1.NE.IPT2.AND.
     +            IPL1.NE.IPT3) GO TO 122
              IF (IPL2.NE.IPT1.AND.IPL2.NE.IPT2.AND.
     +            IPL2.NE.IPT3) GO TO 122
              NTF=NTF+1
              ITF(NTF)=ITT3/3
              IF (NTF.EQ.2) GO TO 123
  122       CONTINUE
C
            IF (NTF.LT.2) GO TO 127
C
C Determine the vertices of the triangles that do not lie
C on the line segment.
C
  123       IT1T3=ITF(1)*3
            IPTI1=IPT(IT1T3-2)
            IF (IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2) GO TO 124
            IPTI1=IPT(IT1T3-1)
            IF (IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2) GO TO 124
            IPTI1=IPT(IT1T3)
  124       IT2T3=ITF(2)*3
            IPTI2=IPT(IT2T3-2)
            IF (IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2) GO TO 125
            IPTI2=IPT(IT2T3-1)
            IF (IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2) GO TO 125
            IPTI2=IPT(IT2T3)
C
C Check if an exchange is necessary.
C
  125       IF (IDXCHG(XD,YD,IPTI1,IPTI2,IPL1,IPL2).EQ.0) GO TO 127
C
C Modify the IPT array when necessary.
C
            IPT(IT1T3-2)=IPTI1
            IPT(IT1T3-1)=IPTI2
            IPT(IT1T3)  =IPL1
            IPT(IT2T3-2)=IPTI2
            IPT(IT2T3-1)=IPTI1
            IPT(IT2T3)  =IPL2
C
C Set new flags.
C
            JWL=JWL+8
            IWL(JWL-7)=IPL1
            IWL(JWL-6)=IPTI1
            IWL(JWL-5)=IPTI1
            IWL(JWL-4)=IPL2
            IWL(JWL-3)=IPL2
            IWL(JWL-2)=IPTI2
            IWL(JWL-1)=IPTI2
            IWL(JWL)  =IPL1
C
            DO 126 JLT3=3,NLT3,3
              IPLJ1=IPL(JLT3-2)
              IPLJ2=IPL(JLT3-1)
              IF ((IPLJ1.EQ.IPL1.AND.IPLJ2.EQ.IPTI2).OR.
     +            (IPLJ2.EQ.IPL1.AND.IPLJ1.EQ.IPTI2))
     +                                  IPL(JLT3)=ITF(1)
              IF ((IPLJ1.EQ.IPL2.AND.IPLJ2.EQ.IPTI1).OR.
     +            (IPLJ2.EQ.IPL2.AND.IPLJ1.EQ.IPTI1))
     +                                  IPL(JLT3)=ITF(2)
  126       CONTINUE
C
  127     CONTINUE
C
          NLFC=NLF
          NLF=JWL/2
          IF (NLF.EQ.NLFC) GO TO 130
C
C Reset the IWL array for the next round.
C
          JWL1MN=2*NLFC+1
          NLFT2=NLF*2
C
          DO 128 JWL1=JWL1MN,NLFT2
            JWL=JWL1+1-JWL1MN
            IWL(JWL)=IWL(JWL1)
  128     CONTINUE
C
          NLF=JWL/2
C
  129   CONTINUE
C
  130 CONTINUE
C
C Re-arrange the IPT array so that the vertices of each triangle
C are listed counter-clockwise.
C
      DO 131 ITT3=3,NTT3,3
        IP1=IPT(ITT3-2)
        IP2=IPT(ITT3-1)
        IP3=IPT(ITT3)
        IF (VPDT(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))
     1                                         .GE.0.0) GO TO 131
        IPT(ITT3-2)=IP2
        IPT(ITT3-1)=IP1
  131 CONTINUE
C
      NT=NT0
      NL=NL0
C
      RETURN
C
      END
