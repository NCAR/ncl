C
C $Id: sfnorm.f,v 1.2 1993-02-24 22:48:03 kennison Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
C
C Declare the dimensions of argument arrays.
C
      DIMENSION XRA(NRA),YRA(NRA),DST(NST),IND(NND)
C
C Declare the point-coordinate arrays.
C
      DIMENSION XPT(100),YPT(100)
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Check for obvious errors in the given dimensions.
C
      IF (.NOT.(NRA.LE.2)) GO TO 10001
      GO TO 10003
10001 CONTINUE
C
      IF (.NOT.(NST.LE.NRA)) GO TO 10004
      GO TO 10006
10004 CONTINUE
C
      IF (.NOT.(NND.LE.NRA)) GO TO 10007
      GO TO 10009
10007 CONTINUE
C
C Convert the desired shading angle to radians.
C
      AIR=.017453292519943*AID
C
C Compute the constants "XCO" and "YCO" such that any line having an
C equation of the form "XCO*x+YCO*y=c" will have the desired angle.
C
      XCO=-SIN(AIR)
      YCO=+COS(AIR)
C
C Compute the spacing between lines.
C
      SBL=DBL
      IF (DBL.LE.0..OR.DBL.GE.1.) SBL=.00125
C
C If lines will be drawn, flush the line buffer.  If points will be
C drawn, re-set the viewport and window and initialize the point buffer.
C
      IF (.NOT.(LPA.EQ.0)) GO TO 10010
      CALL PLOTIF (0.,0.,2)
      GO TO 10011
10010 CONTINUE
      CALL GETSET (FA1,FA2,FA3,FA4,FA5,FA6,FA7,FA8,IA9)
      CALL SET    (FA1,FA2,FA3,FA4,FA1,FA2,FA3,FA4,  1)
      NPT=0
10011 CONTINUE
C
C Compute the directed distances of the given points from the line
C "XCO*x+YCO*y=0".
C
      DO 10012 I=1,NRA
      DST(I)=XCO*XRA(I)+YCO*YRA(I)
10012 CONTINUE
C
C Generate a list of indices of the distances, sorted by increasing
C distance.  DST(IND(1)), DST(IND(2)), ... DST(IND(NRA)) is a list of
C the directed distances of the given points, in increasing numerical
C order.
C
      CALL SFSORT (DST,NRA,0,IND)
C
C Draw lines at distances "DFB" from the baseline "XCO*x+YCO*y=0" which
C are multiples of "SBL" between the smallest and largest point
C distances.  JND points to the index of that point having the greatest
C distance less than the distance of the last line drawn (initially 0)
C and KND points to the end of the list of line segments which the last
C line drawn intersected - it is stored backwards at the end of IND -
C the initial value specifies that this list is empty.
C
      JND=0
      KND=NND+1
C
C IPT is the index of the next point past the last line drawn and IPE
C is the index of the last point.
C
      IPT=IND(1)
      IPE=IND(NRA)
C
      ISP = INT(DST(IPT)/SBL)-1
      GO TO 10015
10013 CONTINUE
      ISP =ISP +1
10015 CONTINUE
      IF (ISP .GT.(INT(DST(IPE)/SBL)+1)) GO TO 10014
C
      JMP=MOD(ISP,8)
      IF (JMP.LT.0) JMP=JMP+8
      JMP=8-JMP
      DFB=REAL(ISP)*SBL
C
C Advance JND to reflect the number of points passed over by the
C algorithm and update the list of pointers to intersecting lines.
C
10016 CONTINUE
      IF (JND.GE.NRA) GO TO 10017
      IF (DFB.LE.DST(IPT)) GO TO 10017
      IPP=MOD(IPT+NRA-2,NRA)+1
      IPF=MOD(IPT      ,NRA)+1
      IF (.NOT.(DST(IPP).LT.DST(IPT))) GO TO 10018
      IPX=IPP
      ASSIGN 10019 TO L10020
      GO TO 10020
10019 CONTINUE
      GO TO 10021
10018 CONTINUE
      IF (.NOT.(DST(IPP).GT.DST(IPT))) GO TO 10022
      IPX=IPP
      ASSIGN 10023 TO L10024
      GO TO 10024
10023 CONTINUE
10021 CONTINUE
10022 CONTINUE
      IF (.NOT.(DST(IPT).GT.DST(IPF))) GO TO 10025
      IPX=IPT
      ASSIGN 10026 TO L10020
      GO TO 10020
10026 CONTINUE
      GO TO 10027
10025 CONTINUE
      IF (.NOT.(DST(IPT).LT.DST(IPF))) GO TO 10028
      IPX=IPT
      ASSIGN 10029 TO L10024
      GO TO 10024
10029 CONTINUE
10027 CONTINUE
10028 CONTINUE
      JND=JND+1
      IPT=IND(JND+1)
      GO TO 10016
10017 CONTINUE
C
C Compute a set of values representing the intersection points of the
C current line with the line segments of the polygon.
C
      IF (.NOT.(KND.LE.NND)) GO TO 10030
      IF (.NOT.(NRA+NND-KND+1.LE.NST)) GO TO 10031
      LND=NRA
      IF (.NOT.(ABS(XCO).GT.ABS(YCO))) GO TO 10032
      DO 10033 I=KND,NND
      IP1=IND(I)
      IP2=MOD(IND(I),NRA)+1
      LND=LND+1
      TMP=XCO*(XRA(IP2)-XRA(IP1))+YCO*(YRA(IP2)-YRA(IP1))
      IF (.NOT.(ABS(TMP).GT..000001)) GO TO 10034
      DST(LND)=(DFB*(YRA(IP2)-YRA(IP1))-XCO*
     +         (XRA(IP1)*YRA(IP2)-XRA(IP2)*YRA(IP1)))/TMP
      GO TO 10035
10034 CONTINUE
      DST(LND)=.5*(YRA(IP1)+YRA(IP2))
10035 CONTINUE
10033 CONTINUE
      GO TO 10036
10032 CONTINUE
      DO 10037 I=KND,NND
      IP1=IND(I)
      IP2=MOD(IND(I),NRA)+1
      LND=LND+1
      TMP=XCO*(XRA(IP2)-XRA(IP1))+YCO*(YRA(IP2)-YRA(IP1))
      IF (.NOT.(ABS(TMP).GT..000001)) GO TO 10038
      DST(LND)=(DFB*(XRA(IP2)-XRA(IP1))+YCO*
     +         (XRA(IP1)*YRA(IP2)-XRA(IP2)*YRA(IP1)))/TMP
      GO TO 10039
10038 CONTINUE
      DST(LND)=.5*(XRA(IP1)+XRA(IP2))
10039 CONTINUE
10037 CONTINUE
10036 CONTINUE
      GO TO 10040
10031 CONTINUE
      GO TO 10006
10040 CONTINUE
C
C Put these values in ascending order.  Actually, once again, we set up
C an index array specifying the order.
C
      IF (.NOT.(LND.LT.KND)) GO TO 10042
      CALL SFSORT (DST(NRA+1),LND-NRA,0,IND(NRA+1))
      GO TO 10043
10042 CONTINUE
      GO TO 10009
10043 CONTINUE
C
C Draw the line segments specified by the list.
C
      IN1=NRA+1
      IF (.NOT.(LPA.EQ.0)) GO TO 10045
      IF (.NOT.(ABS(XCO).GT.ABS(YCO))) GO TO 10046
10047 CONTINUE
      IF (.NOT.(IN1.LT.LND)) GO TO 10048
      JN1=NRA+IND(IN1)
      IN2=IN1+1
10049 CONTINUE
      JN2=NRA+IND(IN2)
      IF (IN2.GE.LND) GO TO 10050
      JNT=NRA+IND(IN2+1)
      IF (DST(JNT)-DST(JN2).GT..000001) GO TO 10050
      IN2=IN2+2
      GO TO 10049
10050 CONTINUE
      IF (.NOT.(DST(JN2)-DST(JN1).GT..000001)) GO TO 10051
      CALL PLOTIF ((DFB-YCO*DST(JN1))/XCO,DST(JN1),0)
      CALL PLOTIF ((DFB-YCO*DST(JN2))/XCO,DST(JN2),1)
10051 CONTINUE
      IN1=IN2+1
      GO TO 10047
10048 CONTINUE
      GO TO 10052
10046 CONTINUE
10053 CONTINUE
      IF (.NOT.(IN1.LT.LND)) GO TO 10054
      JN1=NRA+IND(IN1)
      IN2=IN1+1
10055 CONTINUE
      JN2=NRA+IND(IN2)
      IF (IN2.GE.LND) GO TO 10056
      JNT=NRA+IND(IN2+1)
      IF (DST(JNT)-DST(JN2).GT..000001) GO TO 10056
      IN2=IN2+2
      GO TO 10055
10056 CONTINUE
      IF (.NOT.(DST(JN2)-DST(JN1).GT..000001)) GO TO 10057
      CALL PLOTIF (DST(JN1),(DFB-XCO*DST(JN1))/YCO,0)
      CALL PLOTIF (DST(JN2),(DFB-XCO*DST(JN2))/YCO,1)
10057 CONTINUE
      IN1=IN2+1
      GO TO 10053
10054 CONTINUE
10052 CONTINUE
      GO TO 10058
10045 CONTINUE
      IF (.NOT.(ABS(XCO).GT.ABS(YCO))) GO TO 10059
10060 CONTINUE
      IF (.NOT.(IN1.LT.LND)) GO TO 10061
      JN1=NRA+IND(IN1)
      IN2=IN1+1
10062 CONTINUE
      JN2=NRA+IND(IN2)
      IF (IN2.GE.LND) GO TO 10063
      JNT=NRA+IND(IN2+1)
      IF (DST(JNT)-DST(JN2).GT..000001) GO TO 10063
      IN2=IN2+2
      GO TO 10062
10063 CONTINUE
      IF (.NOT.(DST(JN2)-DST(JN1).GT..000001)) GO TO 10064
      DSA=YCO*(DFB-YCO*DST(JN1))/XCO-XCO*DST(JN1)
      DSB=YCO*(DFB-YCO*DST(JN2))/XCO-XCO*DST(JN2)
      DS1=AMIN1(DSA,DSB)
      DS2=AMAX1(DSA,DSB)
      JS1=INT(DS1/SBL+.5+SIGN(.5,DS1))
      JS2=INT(DS2/SBL-.5+SIGN(.5,DS2))
      JSP = JS1
      GO TO 10067
10065 CONTINUE
      JSP =JSP +1
10067 CONTINUE
      IF (JSP .GT.(JS2)) GO TO 10066
      IMP=MOD(JSP,8)+1
      IF (IMP.LE.0) IMP=IMP+8
      IF (.NOT.(LDP(IMP,JMP).NE.0)) GO TO 10068
      SPJ=REAL(JSP)*SBL
      NPT=NPT+1
      XPT(NPT)=XCO*DFB+YCO*SPJ
      YPT(NPT)=YCO*DFB-XCO*SPJ
      IF (.NOT.(NPT.EQ.100)) GO TO 10069
      CALL POINTS (XPT,YPT,100,LCH,0)
      NPT=0
10069 CONTINUE
10068 CONTINUE
      GO TO 10065
10066 CONTINUE
10064 CONTINUE
      IN1=IN2+1
      GO TO 10060
10061 CONTINUE
      GO TO 10070
10059 CONTINUE
10071 CONTINUE
      IF (.NOT.(IN1.LT.LND)) GO TO 10072
      JN1=NRA+IND(IN1)
      IN2=IN1+1
10073 CONTINUE
      JN2=NRA+IND(IN2)
      IF (IN2.GE.LND) GO TO 10074
      JNT=NRA+IND(IN2+1)
      IF (DST(JNT)-DST(JN2).GT..000001) GO TO 10074
      IN2=IN2+2
      GO TO 10073
10074 CONTINUE
      IF (.NOT.(DST(JN2)-DST(JN1).GT..000001)) GO TO 10075
      DSA=YCO*DST(JN1)-XCO*(DFB-XCO*DST(JN1))/YCO
      DSB=YCO*DST(JN2)-XCO*(DFB-XCO*DST(JN2))/YCO
      DS1=AMIN1(DSA,DSB)
      DS2=AMAX1(DSA,DSB)
      JS1=INT(DS1/SBL+.5+SIGN(.5,DS1))
      JS2=INT(DS2/SBL-.5+SIGN(.5,DS2))
      JSP = JS1
      GO TO 10078
10076 CONTINUE
      JSP =JSP +1
10078 CONTINUE
      IF (JSP .GT.(JS2)) GO TO 10077
      IMP=MOD(JSP,8)+1
      IF (IMP.LE.0) IMP=IMP+8
      IF (.NOT.(LDP(IMP,JMP).NE.0)) GO TO 10079
      SPJ=REAL(JSP)*SBL
      NPT=NPT+1
      XPT(NPT)=XCO*DFB+YCO*SPJ
      YPT(NPT)=YCO*DFB-XCO*SPJ
      IF (.NOT.(NPT.EQ.100)) GO TO 10080
      CALL POINTS (XPT,YPT,100,LCH,0)
      NPT=0
10080 CONTINUE
10079 CONTINUE
      GO TO 10076
10077 CONTINUE
10075 CONTINUE
      IN1=IN2+1
      GO TO 10071
10072 CONTINUE
10070 CONTINUE
10058 CONTINUE
C
10030 CONTINUE
      GO TO 10013
10014 CONTINUE
C
C If lines were drawn, flush the line buffer.  If points were drawn,
C flush the point buffer and restore the original viewport and window.
C
      IF (.NOT.(LPA.EQ.0)) GO TO 10081
      CALL PLOTIF (0.,0.,2)
      GO TO 10082
10081 CONTINUE
      IF (NPT.NE.0) CALL POINTS (XPT,YPT,NPT,LCH,0)
      CALL SET (FA1,FA2,FA3,FA4,FA5,FA6,FA7,FA8,IA9)
10082 CONTINUE
C
C Done.
C
      RETURN
C
C Remove the line segment numbered IPX from the list of intersecting
C line segments.
C
10020 CONTINUE
      IF (.NOT.(KND.LE.NND)) GO TO 10083
      DO 10084 I=KND,NND
      IF (.NOT.(IND(I).EQ.IPX)) GO TO 10085
      IND(I)=IND(KND)
      KND=KND+1
      GO TO 101
10085 CONTINUE
10084 CONTINUE
      GO TO 10087
10083 CONTINUE
      GO TO 10087
  101 CONTINUE
      GO TO L10020 , (10026,10019)
C
C Add the line segment numbered IPX to the list of intersecting line
C line segments.
C
10024 CONTINUE
      KND=KND-1
      IF (.NOT.(KND.GT.NRA)) GO TO 10089
      IND(KND)=IPX
      GO TO 10090
10089 CONTINUE
      GO TO 10009
10090 CONTINUE
      GO TO L10024 , (10029,10023)
C
C Error exits.
C
10003 CONTINUE
      CALL SETER ('SFNORM - COORDINATE ARRAYS TOO SMALL',1,2)
      STOP
C
10006 CONTINUE
      CALL SETER ('SFNORM - ARRAY DST IS TOO SMALL',2,2)
      STOP
C
10009 CONTINUE
      CALL SETER ('SFNORM - ARRAY IND IS TOO SMALL',3,2)
      STOP
C
10087 CONTINUE
      CALL SETER ('SFNORM - LOGIC ERROR - SEE SPECIALIST',4,2)
      STOP
C
      END
