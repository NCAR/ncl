C
C	$Id: arpram.f,v 1.1.1.1 1992-04-17 22:32:13 ncargd Exp $
C
C
C The subroutine ARPRAM.
C --- ---------- -------
C
      SUBROUTINE ARPRAM (IAM,IF1,IF2,IF3)
C
      DIMENSION IAM(*)
C
C Examine the area map.  Find points of intersection, delete redundant
C edge segments, and adjust the area identifiers.
C
C IAM is the array which holds an area map previously initialized by a
C call to ARINAM and augmented by calls to AREDAM.
C
C IF1 specifies what kind of search is made for intersections.  If IF1
C is zero, all pairs of edge segments which could possibly intersect
C are examined for actual intersection, a very time-consuming process.
C If IF1 is one, a pair is examined only if one of its members has a
C left or a right identifier less than or equal to zero; this saves a
C lot of time and is intended for use with contour lines.
C
C IF2 specifies what kind of action is taken to remove unclosed edges.
C If IF2 is zero, a search is made for such edges and they are simply
C removed from the area map.  If IF2 is one, no such search is made;
C all edges are assumed closed.
C
C IF3 specifies what kind of search is made for area identifiers to be
C changed.  If IF3 is zero, all edges of all subareas are examined in
C complete detail, a very time-consuming process.  If IF3 is one, only
C those edges having a zero or negative area identifier are examined
C (all others being assumed correct) and holes are ignored, which saves
C a lot of time; this is intended for use with contour lines.
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI
      SAVE   /ARCOMN/
C
C Declare the BLOCK DATA routine external, which should force it to
C load from a binary library.
C
      EXTERNAL ARBLDA
C
C Define some double precision variables.
C
      DOUBLE PRECISION DPT,DP1,DP2,DX0,DY0,DLP
C
C Define the arrays which determine the multiple-precision operations
C to be done by ARMPIA.
C
      DIMENSION IO1(4,8),IO2(4,4),IO3(4,8),IO4(4,18),IO5(4,4)
      DIMENSION IO6(4,8),IO7(4,4)
C
      DATA IO1 / 1 ,  1 ,  0 ,  0 ,
     +           1 ,  2 ,  0 ,  0 ,
     +           1 ,  3 ,  0 ,  0 ,
     +           1 ,  4 ,  0 ,  0 ,
     +           4 ,  5 ,  1 ,  3 ,
     +           4 ,  6 ,  2 ,  4 ,
     +           2 ,  7 ,  5 ,  6 ,
     +           5 ,  7 ,  0 ,  0 /
      DATA IO2 / 4 ,  3 ,  1 ,  1 ,
     +           4 ,  4 ,  2 ,  2 ,
     +           2 ,  5 ,  3 ,  4 ,
     +           5 ,  5 ,  0 ,  0 /
      DATA IO3 / 1 ,  1 ,  0 ,  0 ,
     +           1 ,  2 ,  0 ,  0 ,
     +           1 ,  3 ,  0 ,  0 ,
     +           1 ,  4 ,  0 ,  0 ,
     +           4 ,  5 ,  1 ,  2 ,
     +           4 ,  6 ,  3 ,  4 ,
     +           3 ,  7 ,  5 ,  6 ,
     +           5 ,  7 ,  0 ,  0 /
      DATA IO4 / 1 ,  7 ,  0 ,  0 ,
     +           1 ,  8 ,  0 ,  0 ,
     +           4 ,  9 ,  7 ,  8 ,
     +           1 ,  7 ,  0 ,  0 ,
     +           1 ,  8 ,  0 ,  0 ,
     +           4 , 10 ,  7 ,  8 ,
     +           3 ,  5 ,  9 , 10 ,
     +           1 ,  7 ,  0 ,  0 ,
     +           1 ,  8 ,  0 ,  0 ,
     +           4 ,  9 ,  7 ,  8 ,
     +           1 ,  7 ,  0 ,  0 ,
     +           1 ,  8 ,  0 ,  0 ,
     +           4 , 10 ,  7 ,  8 ,
     +           3 ,  6 ,  9 , 10 ,
     +           4 ,  7 ,  3 ,  5 ,
     +           4 ,  8 ,  1 ,  6 ,
     +           3 ,  9 ,  7 ,  8 ,
     +           5 ,  9 ,  0 ,  0 /
      DATA IO5 / 4 ,  7 ,  2 ,  5 ,
     +           4 ,  8 ,  4 ,  6 ,
     +           3 ,  9 ,  7 ,  8 ,
     +           5 ,  9 ,  0 ,  0 /
      DATA IO6 / 1 ,  1 ,  0 ,  0 ,
     +           1 ,  2 ,  0 ,  0 ,
     +           1 ,  3 ,  0 ,  0 ,
     +           1 ,  4 ,  0 ,  0 ,
     +           4 ,  5 ,  1 ,  2 ,
     +           4 ,  6 ,  3 ,  4 ,
     +           3 ,  7 ,  5 ,  6 ,
     +           5 ,  7 ,  0 ,  0 /
      DATA IO7 / 4 ,  5 ,  1 ,  4 ,
     +           4 ,  6 ,  2 ,  3 ,
     +           2 ,  7 ,  5 ,  6 ,
     +           5 ,  7 ,  0 ,  0 /
C
C Pull out the length of the area map and check for initialization.
C
      LAM=IAM(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAM(LAM).NE.LAM)) GO TO 10001
        CALL SETER ('ARPRAM - INITIALIZATION DONE IMPROPERLY',1,1)
        RETURN
10001 CONTINUE
C
C Get a double-precision version of ILP.
C
      DLP=DBLE(ILP)
C
C Copy the fast-path flags to internal variables.
C
      KF1=IF1
      KF2=IF2
      KF3=IF3
C
C Initialize the pointer used in determining the coordinate ordering of
C new nodes.
C
      IPX=8
C
C If debugging is turned on, produce an initial plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10002
        CALL ARDBPA (IAM,IDB,'AT START OF ARPRAM')
10002 CONTINUE
C
C First, find the average length of the projection on the x axis of the
C line segments in the area map.
C
C
      NXL=0
      FXL=0.
      IPT=8
10003 CONTINUE
      IF (.NOT.(IAM(IPT+3).NE.18)) GO TO 10004
        IPT=IAM(IPT+3)
        IF (.NOT.(IAM(IPT+7).NE.0)) GO TO 10005
          NXL=NXL+1
          FXL=FXL+REAL(ABS(IAM(IPT+1)-IAM(IAM(IPT+4)+1)))
10005   CONTINUE
      GO TO 10003
10004 CONTINUE
      IF (.NOT.(NXL.EQ.0)) GO TO 10006
        CALL SETER ('ARPRAM - NO EDGES IN AREA MAP',2,1)
        RETURN
10006 CONTINUE
      IXL=INT(FXL/REAL(NXL))
C
C Decide what the maximum such length should be and save it.  Adjust
C the x and y coordinates in the dummy nodes at the ends of the list.
C
      IAM(2)=MAX(2,MIN(ILC,2*IXL))
      IAM(9)=-(IAM(2)+1)
      IAM(10)=-(IAM(2)+1)
      IAM(19)=ILC+(IAM(2)+1)
      IAM(20)=ILC+(IAM(2)+1)
C
C
C Now, break up any edge segments whose projections on the x axis are
C greater than the maximum.
C
C
      IPI=8
10007 CONTINUE
      IF (.NOT.(IAM(IPI+3).NE.18)) GO TO 10008
        IPI=IAM(IPI+3)
        IF (.NOT.(IAM(IPI+7).NE.0)) GO TO 10009
          NDO=1+(ABS(IAM(IPI+1)-IAM(IAM(IPI+4)+1))-1)/
     +                                              IAM(2)
          IF (.NOT.(NDO.GT.1)) GO TO 10010
            IXL=IAM(IAM(IPI+4)+1)
            IYL=IAM(IAM(IPI+4)+2)
            FXD=REAL(IAM(IPI+1)-IXL)/REAL(NDO)
            FYD=REAL(IAM(IPI+2)-IYL)/REAL(NDO)
              I = 1
              GO TO 10013
10011         CONTINUE
              I =I +1
10013         CONTINUE
              IF (I .GT.(NDO-1)) GO TO 10012
              IX0=IXL+NINT(REAL(I)*FXD)
              IY0=IYL+NINT(REAL(I)*FYD)
              ASSIGN 10014 TO L10015
              GO TO 10015
10014         CONTINUE
            GO TO 10011
10012       CONTINUE
10010     CONTINUE
10009   CONTINUE
      GO TO 10007
10008 CONTINUE
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10016
        CALL ARDBPA (IAM,IDB,'AFTER BREAKING UP LONG EDGE SEGMENTS')
10016 CONTINUE
C
C
C Now, look for points of intersection between edges.  The algorithm
C used should run in O(nlogn) time, rather than O(n**2) time.  A
C vertical sweep line is passed over the plane from left to right.
C A list is kept of all the edge segments intersected by the sweep
C line, sorted in order of increasing Y coordinate.  Whenever a new
C edge segment is added to the list, a check is made for intersection
C of it with the edge segments on either side of it.  Whenever an edge
C segment is removed from the list, a check is made for intersection
C of the edge segments which were on either side of it.  There are
C various complications, mostly having to do with the fact that points
C of intersection are incorporated in the area map as they are found
C and that we are working with a discrete grid, rather than with the
C classic Euclidean plane.
C
C The list is kept at the upper end of the area map.  Each five-word
C node in it represents one edge segment intersecting the sweep line
C and has the following format:
C
C   Word 0:  A pointer to the next node in the list (0 if no more).
C   Word 1:  A pointer to the last node in the list (0 if no more).
C   Word 2:  A pointer to the left end of the edge segment.
C   Word 3:  A pointer to the right end of the edge segment.
C   Word 4:  A pointer to the drawing end of the edge segment.
C
  101 CONTINUE
C
C Save the index of the last element used at the upper end of the area
C map.
C
      ISU=IAM(6)
C
C IQB points to the beginning of the edge-segment list and IQE to a
C linked list of nodes removed from the edge-segment list and available
C for re-use.  Both start out empty.
C
      IQB=0
      IQE=0
C
C ISP points to the node in the area map defining the current position
C of the sweep line.
C
      ISP=8
C
C A WHILE loop moves the sweep line, one point position at a time,
C across the area map.
C
10017 CONTINUE
      IF (.NOT.(IAM(ISP+5).NE.18)) GO TO 10018
C
        ISP=IAM(ISP+5)
C
C Pull out the x and y coordinates of the point defining the current
C position of the sweep line.
C
        ISX=IAM(ISP+1)
        FSX=REAL(ISX)
        ISY=IAM(ISP+2)
        FSY=REAL(ISY)
C
C Interpolate the new point along any line segment which passes within
C one unit of it and, at the same time, remove any line segment having
C the current point, or any with the same coordinates, as a right
C endpoint.
C
        IQT=IQB
C
10019   CONTINUE
C
          IF (IQT.EQ.0) GO TO 10020
C
          IX1=IAM(IAM(IQT+2)+1)
          FX1=REAL(IX1)
          IY1=IAM(IAM(IQT+2)+2)
          FY1=REAL(IY1)
          IX2=IAM(IAM(IQT+3)+1)
          FX2=REAL(IX2)
          IY2=IAM(IAM(IQT+3)+2)
          FY2=REAL(IY2)
C
          IF (.NOT.(IAU.EQ.1)) GO TO 10021
            TMP=MAX(0.,MIN(1.,
     +          ((FSX-FX1)*(FX2-FX1)+(FSY-FY1)*(FY2-FY1))/
     +          ((FX2-FX1)*(FX2-FX1)+(FY2-FY1)*(FY2-FY1))))
            DSQ=(FX1-FSX+(FX2-FX1)*TMP)**2+(FY1-FSY+(FY2-FY1)*TMP)**2
          GO TO 10022
10021     CONTINUE
          IF (.NOT.(IAU.EQ.2)) GO TO 10023
            DPT=MAX(0.D0,MIN(1.D0,(DBLE(ISX-IX1)*DBLE(IX2-IX1)+
     +                             DBLE(ISY-IY1)*DBLE(IY2-IY1))/
     +                            (DBLE(IX2-IX1)*DBLE(IX2-IX1)+
     +                             DBLE(IY2-IY1)*DBLE(IY2-IY1))))
            DSQ=REAL((DBLE(IX1-ISX)+DBLE(IX2-IX1)*DPT)**2+
     +               (DBLE(IY1-ISY)+DBLE(IY2-IY1)*DPT)**2)
          GO TO 10022
10023     CONTINUE
            IO1(3,1)=IX2-IX1
            IO1(3,2)=IY2-IY1
            IO1(3,3)=ISX-IX1
            IO1(3,4)=ISY-IY1
            CALL ARMPIA (IO1,DP1)
            CALL ARMPIA (IO2,DP2)
            DPT=MAX(0.D0,MIN(1.D0,DP1/DP2))
            DSQ=REAL((DBLE(IX1-ISX)+DBLE(IX2-IX1)*DPT)**2+
     +               (DBLE(IY1-ISY)+DBLE(IY2-IY1)*DPT)**2)
10022     CONTINUE
C
          IF (.NOT.((DSQ.LT.1.).AND.(IX1.NE.ISX.OR.IY1.NE.ISY).AND.(IX2.
     +NE.ISX.OR.IY2.NE.ISY))) GO TO 10024
            IPI=IAM(IQT+4)
            IX0=ISX
            IY0=ISY
            ASSIGN 10025 TO L10015
            GO TO 10015
10025       CONTINUE
            IAM(IQT+3)=IPN
            IF (IAM(IQT+4).NE.IAM(IQT+2)) IAM(IQT+4)=IPN
10024     CONTINUE
C
          IF (.NOT.(IAM(IAM(IQT+3)+1).EQ.ISX.AND.IAM(IAM(IQT+3)+2).EQ.IS
     +Y)) GO TO 10026
            IF (.NOT.(IAM(IQT+1).EQ.0)) GO TO 10027
              IQB=IAM(IQT)
              IF (IQB.NE.0) IAM(IQB+1)=0
              IAM(IQT)=IQE
              IQE=IQT
              IQT=IQB
            GO TO 10028
10027       CONTINUE
            IF (.NOT.(IAM(IQT).EQ.0)) GO TO 10029
              IAM(IAM(IQT+1))=0
              IAM(IQT)=IQE
              IQE=IQT
              IQT=0
            GO TO 10028
10029       CONTINUE
              IQL=IAM(IQT+1)
              IQU=IAM(IQT)
              IAM(IQL)=IQU
              IAM(IQU+1)=IQL
              IAM(IQT)=IQE
              IQE=IQT
              IF (.NOT.(IAM(IAM(IQU+3)+1).NE.ISX.OR.IAM(IAM(IQU+3)+2).NE
     +.ISY))  GO TO 10030
                ASSIGN 10031 TO L10032
                GO TO 10032
10031           CONTINUE
10030         CONTINUE
              IQT=IQU
10028       CONTINUE
          GO TO 10033
10026     CONTINUE
            IQT=IAM(IQT)
10033     CONTINUE
C
        GO TO 10019
10020   CONTINUE
C
C Add line segments having the current point as a left endpoint.
C
        IF (.NOT.(IAM(ISP+7).NE.0.AND.(IAM(ISP+1).LT.IAM(IAM(ISP+4)+1).O
     +R.(IAM(ISP+1).EQ.IAM(IAM(ISP+4)+1).AND.IAM(ISP+2).LT.IAM(IAM(ISP+4
     +)+2))))) GO TO 10034
          ISL=ISP
          ISR=IAM(ISP+4)
          ISD=ISP
          ASSIGN 10035 TO L10036
          GO TO 10036
10035     CONTINUE
10034   CONTINUE
C
        IF (.NOT.(IAM(IAM(ISP+3)+7).NE.0.AND.(IAM(ISP+1).LT.IAM(IAM(ISP+
     +3)+1).OR.(IAM(ISP+1).EQ.IAM(IAM(ISP+3)+1).AND.IAM(ISP+2).LT.IAM(IA
     +M(ISP+3)+2))))) GO TO 10037
          ISL=ISP
          ISR=IAM(ISP+3)
          ISD=ISR
          ASSIGN 10038 TO L10036
          GO TO 10036
10038     CONTINUE
10037   CONTINUE
C
      GO TO 10017
10018 CONTINUE
C
C The following internal procedure adds a segment to the list.  It also
C checks for intersection of that segment with the segments on either
C side of it in the list.
C
      GO TO 10039
10036 CONTINUE
C
        IQS=0
        IQT=IQB
        ILX=IAM(ISL+1)
        RLY=REAL(IAM(ISL+2))
C
        IF (.NOT.(IAM(ISL+1).NE.IAM(ISR+1))) GO TO 10040
          SLL=REAL(IAM(ISR+2)-IAM(ISL+2))/
     +  REAL(IAM(ISR+1)-IAM(ISL+1))
        GO TO 10041
10040   CONTINUE
          SLL=RLP
10041   CONTINUE
C
10042   CONTINUE
          IF (IQT.EQ.0) GO TO 10043
          IF (.NOT.(IAM(IAM(IQT+2)+1).NE.IAM(IAM(IQT+3)+1))) GO TO 10044
            IF (.NOT.(IAU.EQ.1)) GO TO 10045
              SLP=REAL(IAM(IAM(IQT+3)+2)-IAM(IAM(IQT+2)+2))/
     +      REAL(IAM(IAM(IQT+3)+1)-IAM(IAM(IQT+2)+1))
              RTY=REAL(IAM(IAM(IQT+2)+2))+
     +         REAL(ILX-IAM(IAM(IQT+2)+1))*SLP
            GO TO 10046
10045       CONTINUE
              DPT=DBLE(IAM(IAM(IQT+3)+2)-IAM(IAM(IQT+2)+2))/
     +      DBLE(IAM(IAM(IQT+3)+1)-IAM(IAM(IQT+2)+1))
              SLP=REAL(DPT)
              RTY=DBLE(IAM(IAM(IQT+2)+2))+
     +         DBLE(ILX-IAM(IAM(IQT+2)+1))*DPT
10046       CONTINUE
          GO TO 10047
10044     CONTINUE
            SLP=RLP
            RTY=REAL(IAM(IAM(IQT+2)+2))
10047     CONTINUE
          IF (RLY.LT.RTY.OR.(RLY.EQ.RTY.AND.SLL.LT.SLP)) GO TO 10043
          IQS=IQT
          IQT=IAM(IQT)
        GO TO 10042
10043   CONTINUE
C
        IF (.NOT.(IQE.NE.0)) GO TO 10048
          IQI=IQE
          IQE=IAM(IQE)
        GO TO 10049
10048   CONTINUE
          IF (.NOT.(IAM(6)-5.LE.IAM(5))) GO TO 10050
            CALL SETER ('ARPRAM - AREA-MAP ARRAY OVERFLOW',3,1)
            RETURN
10050     CONTINUE
          IQI=IAM(6)-5
          IAM(6)=IQI
10049   CONTINUE
C
        IAM(IQI)=IQT
        IAM(IQI+1)=IQS
        IAM(IQI+2)=ISL
        IAM(IQI+3)=ISR
        IAM(IQI+4)=ISD
C
        IF (IQS.NE.0) IAM(IQS)=IQI
        IF (IQT.NE.0) IAM(IQT+1)=IQI
C
        IF (IQB.EQ.IQT) IQB=IQI
C
        IF (.NOT.(IQS.NE.0)) GO TO 10051
          IQL=IQS
          IQU=IQI
          ASSIGN 10052 TO L10032
          GO TO 10032
10052     CONTINUE
10051   CONTINUE
C
        IF (.NOT.(IQT.NE.0)) GO TO 10053
          IQL=IQI
          IQU=IQT
          ASSIGN 10054 TO L10032
          GO TO 10032
10054     CONTINUE
10053   CONTINUE
C
      GO TO L10036 , (10038,10035)
10039 CONTINUE
C
C The following internal procedure looks for intersection between the
C segments identified by the pointers IQL and IQU.  When such points of
C intersection are found, they are interpolated along the intersecting
C line segments and any line segments which pass within one unit.
C
      GO TO 10055
10032 CONTINUE
C
        IF (.NOT.(KF1.EQ.0.OR.IAM(IAM(IQL+4)+8).LE.0.OR.IAM(IAM(IQL+4)+9
     +).LE.0.OR.IAM(IAM(IQU+4)+8).LE.0.OR.IAM(IAM(IQU+4)+9).LE.0))
     +  GO TO 10056
C
          IX1=IAM(IAM(IQL+2)+1)
          FX1=REAL(IX1)
          IY1=IAM(IAM(IQL+2)+2)
          FY1=REAL(IY1)
          IX2=IAM(IAM(IQL+3)+1)
          FX2=REAL(IX2)
          IY2=IAM(IAM(IQL+3)+2)
          FY2=REAL(IY2)
          IX3=IAM(IAM(IQU+2)+1)
          FX3=REAL(IX3)
          IY3=IAM(IAM(IQU+2)+2)
          FY3=REAL(IY3)
          IX4=IAM(IAM(IQU+3)+1)
          FX4=REAL(IX4)
          IY4=IAM(IAM(IQU+3)+2)
          FY4=REAL(IY4)
C
          IF (.NOT.(IAU.EQ.1)) GO TO 10057
            TMP=(FX2-FX1)*(FY4-FY3)-(FX4-FX3)*(FY2-FY1)
          GO TO 10058
10057     CONTINUE
          IF (.NOT.(IAU.EQ.2)) GO TO 10059
            DPT=DBLE(IX2-IX1)*DBLE(IY4-IY3)-
     +          DBLE(IX4-IX3)*DBLE(IY2-IY1)
            TMP=REAL(DPT)
          GO TO 10058
10059     CONTINUE
            IO3(3, 1)=IX2-IX1
            IO3(3, 2)=IY4-IY3
            IO3(3, 3)=IX4-IX3
            IO3(3, 4)=IY2-IY1
            CALL ARMPIA (IO3,DPT)
            TMP=REAL(DPT)
10058     CONTINUE
C
          IF (.NOT.(ABS(TMP).GT..1)) GO TO 10060
C
            IF (.NOT.(IAU.EQ.1)) GO TO 10061
              IX0=INT(MAX(-1.,MIN(RLP,
     +                ((FX4-FX3)*(FX2*FY1-FX1*FY2)-
     +                 (FX2-FX1)*(FX4*FY3-FX3*FY4))/TMP+.5)))
            GO TO 10062
10061       CONTINUE
            IF (.NOT.(IAU.EQ.2)) GO TO 10063
              IX0=INT(MAX(-1.D0,MIN(DLP,
     +                (DBLE(IX4-IX3)*
     +                (DBLE(IX2)*DBLE(IY1)-DBLE(IX1)*DBLE(IY2))-
     +                 DBLE(IX2-IX1)*
     +                (DBLE(IX4)*DBLE(IY3)-DBLE(IX3)*DBLE(IY4)))/
     +                                                     DPT+.5D0)))
            GO TO 10062
10063       CONTINUE
              IO4(3, 1)=IX2
              IO4(3, 2)=IY1
              IO4(3, 4)=IX1
              IO4(3, 5)=IY2
              IO4(3, 8)=IX4
              IO4(3, 9)=IY3
              IO4(3,11)=IX3
              IO4(3,12)=IY4
              CALL ARMPIA (IO4,DX0)
              IX0=INT(MAX(-1.D0,MIN(DLP,DX0/DPT+.5D0)))
10062       CONTINUE
C
            IF (.NOT.(IAU.EQ.1)) GO TO 10064
              IY0=INT(MAX(-1.,MIN(RLP,
     +                ((FY4-FY3)*(FX2*FY1-FX1*FY2)-
     +                 (FY2-FY1)*(FX4*FY3-FX3*FY4))/TMP+.5)))
            GO TO 10065
10064       CONTINUE
            IF (.NOT.(IAU.EQ.2)) GO TO 10066
              IY0=INT(MAX(-1.D0,MIN(DLP,
     +                (DBLE(IY4-IY3)*
     +                (DBLE(IX2)*DBLE(IY1)-DBLE(IX1)*DBLE(IY2))-
     +                 DBLE(IY2-IY1)*
     +                (DBLE(IX4)*DBLE(IY3)-DBLE(IX3)*DBLE(IY4)))/
     +                                                     DPT+.5D0)))
            GO TO 10065
10066       CONTINUE
              CALL ARMPIA (IO5,DY0)
              IY0=INT(MAX(-1.D0,MIN(DLP,DY0/DPT+.5D0)))
10065       CONTINUE
C
            IF (IX0.EQ.ISX.AND.IY0.LT.ISY) IX0=IX0+1
C
            FX0=REAL(IX0)
            FY0=REAL(IY0)
C
            IF (.NOT.((FX0-FX1)*(FX0-FX2).LE.0..AND.(FY0-FY1)*(FY0-FY2).
     +LE.0..AND.(FX0-FX3)*(FX0-FX4).LE.0..AND.(FY0-FY3)*(FY0-FY4).LE.0.)
     +)     GO TO 10067
C
              INP=0
C
              IF (.NOT.((IX0.NE.IX1.OR.IY0.NE.IY1).AND.(IX0.NE.IX2.OR.IY
     +0.NE.IY2))) GO TO 10068
                INP=1
                IPI=IAM(IQL+4)
                ASSIGN 10069 TO L10015
                GO TO 10015
10069           CONTINUE
                IAM(IQL+3)=IPN
                IF (IAM(IQL+4).NE.IAM(IQL+2)) IAM(IQL+4)=IPN
10068         CONTINUE
C
              IF (.NOT.((IX0.NE.IX3.OR.IY0.NE.IY3).AND.(IX0.NE.IX4.OR.IY
     +0.NE.IY4))) GO TO 10070
                INP=1
                IPI=IAM(IQU+4)
                ASSIGN 10071 TO L10015
                GO TO 10015
10071           CONTINUE
                IAM(IQU+3)=IPN
                IF (IAM(IQU+4).NE.IAM(IQU+2)) IAM(IQU+4)=IPN
10070         CONTINUE
C
              IF (.NOT.(INP.NE.0)) GO TO 10072
                IQT=IQB
10073           CONTINUE
                  IF (IQT.EQ.0) GO TO 10074
                  IX1=IAM(IAM(IQT+2)+1)
                  FX1=REAL(IX1)
                  IY1=IAM(IAM(IQT+2)+2)
                  FY1=REAL(IY1)
                  IX2=IAM(IAM(IQT+3)+1)
                  FX2=REAL(IX2)
                  IY2=IAM(IAM(IQT+3)+2)
                  FY2=REAL(IY2)
C
                  IF (.NOT.(IAU.EQ.1)) GO TO 10075
                    TMP=MAX(0.,MIN(1.,
     +                  ((FX0-FX1)*(FX2-FX1)+(FY0-FY1)*(FY2-FY1))/
     +                  ((FX2-FX1)*(FX2-FX1)+(FY2-FY1)*(FY2-FY1))))
                    DSQ=(FX1-FX0+(FX2-FX1)*TMP)**2+
     +                  (FY1-FY0+(FY2-FY1)*TMP)**2
                  GO TO 10076
10075             CONTINUE
                  IF (.NOT.(IAU.EQ.2)) GO TO 10077
                    DPT=MAX(0.D0,MIN(1.D0,
     +                                 (DBLE(IX0-IX1)*DBLE(IX2-IX1)+
     +                                  DBLE(IY0-IY1)*DBLE(IY2-IY1))/
     +                                 (DBLE(IX2-IX1)*DBLE(IX2-IX1)+
     +                                  DBLE(IY2-IY1)*DBLE(IY2-IY1))))
                    DSQ=(DBLE(IX1-IX0)+DBLE(IX2-IX1)*DPT)**2+
     +                  (DBLE(IY1-IY0)+DBLE(IY2-IY1)*DPT)**2
                  GO TO 10076
10077             CONTINUE
                    IO1(3,1)=IX2-IX1
                    IO1(3,2)=IY2-IY1
                    IO1(3,3)=IX0-IX1
                    IO1(3,4)=IY0-IY1
                    CALL ARMPIA (IO1,DP1)
                    CALL ARMPIA (IO2,DP2)
                    DPT=MAX(0.D0,MIN(1.D0,DP1/DP2))
                    DSQ=(DBLE(IX1-IX0)+DBLE(IX2-IX1)*DPT)**2+
     +                  (DBLE(IY1-IY0)+DBLE(IY2-IY1)*DPT)**2
10076             CONTINUE
C
                  IF (.NOT.((DSQ.LT.1.).AND.(IX1.NE.IX0.OR.IY1.NE.IY0).A
     +ND.(IX2.NE.IX0.OR.IY2.NE.IY0))) GO TO 10078
                    IPI=IAM(IQT+4)
                    ASSIGN 10079 TO L10015
                    GO TO 10015
10079               CONTINUE
                    IAM(IQT+3)=IPN
                    IF (IAM(IQT+4).NE.IAM(IQT+2)) IAM(IQT+4)=IPN
10078             CONTINUE
                  IQT=IAM(IQT)
                GO TO 10073
10074           CONTINUE
10072         CONTINUE
C
10067       CONTINUE
C
10060     CONTINUE
C
10056   CONTINUE
C
      GO TO L10032 , (10054,10052,10031)
10055 CONTINUE
C
C Restore the index of the last element used at the upper end of the
C area map.
C
      IAM(6)=ISU
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10080
        CALL ARDBPA (IAM,IDB,'AFTER FINDING POINTS OF INTERSECTION')
10080 CONTINUE
C
C
C Now look for coincident line segments in the list and remove them.
C
      ISP=8
10081 CONTINUE
      IF (.NOT.(IAM(ISP+5).NE.18)) GO TO 10082
        ISP=IAM(ISP+5)
        IF (.NOT.(IAM(ISP+7).NE.0)) GO TO 10083
          IF (.NOT.(IAM(IAM(ISP+3)+7).NE.0.AND.IAM(IAM(ISP+3)+1).EQ.IAM(
     +IAM(ISP+4)+1).AND.IAM(IAM(ISP+3)+2).EQ.IAM(IAM(ISP+4)+2))) GO TO 1
     +0084
            IP1=ISP
            IP2=IAM(ISP+3)
            ASSIGN 10085 TO L10086
            GO TO 10086
10085       CONTINUE
10084     CONTINUE
          ISQ=IAM(ISP+5)
10087     CONTINUE
          IF (.NOT.(IAM(ISQ+1).EQ.IAM(ISP+1).AND.IAM(ISQ+2).EQ.IAM(ISP+2
     +))) GO TO 10088
            IF (.NOT.(IAM(ISQ+7).NE.0.AND.IAM(IAM(ISQ+4)+1).EQ.IAM(IAM(I
     +SP+4)+1).AND.IAM(IAM(ISQ+4)+2).EQ.IAM(IAM(ISP+4)+2))) GO TO 10089
              IP1=ISP
              IP2=ISQ
              ASSIGN 10090 TO L10086
              GO TO 10086
10090         CONTINUE
10089       CONTINUE
            IF (.NOT.(IAM(IAM(ISQ+3)+7).NE.0.AND.IAM(IAM(ISQ+3)+1).EQ.IA
     +M(IAM(ISP+4)+1).AND.IAM(IAM(ISQ+3)+2).EQ.IAM(IAM(ISP+4)+2))) GO TO
     + 10091
              IP1=ISP
              IP2=IAM(ISQ+3)
              ASSIGN 10092 TO L10086
              GO TO 10086
10092         CONTINUE
10091       CONTINUE
            ISQ=IAM(ISQ+5)
          GO TO 10087
10088     CONTINUE
10083   CONTINUE
        IF (.NOT.(IAM(IAM(ISP+3)+7).NE.0)) GO TO 10093
          ISQ=IAM(ISP+5)
10094     CONTINUE
          IF (.NOT.(IAM(ISQ+1).EQ.IAM(ISP+1).AND.IAM(ISQ+2).EQ.IAM(ISP+2
     +))) GO TO 10095
            IF (.NOT.(IAM(ISQ+7).NE.0.AND.IAM(IAM(ISQ+4)+1).EQ.IAM(IAM(I
     +SP+3)+1).AND.IAM(IAM(ISQ+4)+2).EQ.IAM(IAM(ISP+3)+2))) GO TO 10096
              IP1=IAM(ISP+3)
              IP2=ISQ
              ASSIGN 10097 TO L10086
              GO TO 10086
10097         CONTINUE
10096       CONTINUE
            IF (.NOT.(IAM(IAM(ISQ+3)+7).NE.0.AND.IAM(IAM(ISQ+3)+1).EQ.IA
     +M(IAM(ISP+3)+1).AND.IAM(IAM(ISQ+3)+2).EQ.IAM(IAM(ISP+3)+2))) GO TO
     + 10098
              IP1=IAM(ISP+3)
              IP2=IAM(ISQ+3)
              ASSIGN 10099 TO L10086
              GO TO 10086
10099         CONTINUE
10098       CONTINUE
            ISQ=IAM(ISQ+5)
          GO TO 10094
10095     CONTINUE
10093   CONTINUE
      GO TO 10081
10082 CONTINUE
C
C This internal procedure processes coincident pairs of segments found.
C If both members of the pair belong to the same group, area identifier
C information from both members is ignored and one of the pair is
C deleted.  If they belong to different groups, the group id in one of
C them is negated, so that it is present when we are looking at edges
C belonging to a single group, but absent when we are looking at all
C the edges together.
C
      GO TO 10100
10086 CONTINUE
        IF (.NOT.(ABS(IAM(IP1+7)).EQ.ABS(IAM(IP2+7)))) GO TO 10101
          IL1=IAM(IP1+8)
          IR1=IAM(IP1+9)
          IF (.NOT.(IAM(IP1+1).EQ.IAM(IP2+1).AND.IAM(IP1+2).EQ.IAM(IP2+2
     +))) GO TO 10102
            IL2=IAM(IP2+8)
            IR2=IAM(IP2+9)
          GO TO 10103
10102     CONTINUE
            IL2=IAM(IP2+9)
            IR2=IAM(IP2+8)
10103     CONTINUE
          IMN=MIN(IL1,IL2)
          IMX=MAX(IL1,IL2)
          IF (.NOT.(IMN.LT.0)) GO TO 10104
            IF (.NOT.(IMX.LT.0)) GO TO 10105
              IAM(IP1+8)=-1
            GO TO 10106
10105       CONTINUE
              IAM(IP1+8)=0
10106       CONTINUE
          GO TO 10107
10104     CONTINUE
          IF (.NOT.(IMN.EQ.0)) GO TO 10108
            IAM(IP1+8)=IMX
          GO TO 10107
10108     CONTINUE
          IF (.NOT.(IMN.EQ.IMX)) GO TO 10109
            IAM(IP1+8)=IMN
          GO TO 10107
10109     CONTINUE
            IAM(IP1+8)=0
10107     CONTINUE
          IMN=MIN(IR1,IR2)
          IMX=MAX(IR1,IR2)
          IF (.NOT.(IMN.LT.0)) GO TO 10110
            IF (.NOT.(IMX.LT.0)) GO TO 10111
              IAM(IP1+9)=-1
            GO TO 10112
10111       CONTINUE
              IAM(IP1+9)=0
10112       CONTINUE
          GO TO 10113
10110     CONTINUE
          IF (.NOT.(IMN.EQ.0)) GO TO 10114
            IAM(IP1+9)=IMX
          GO TO 10113
10114     CONTINUE
          IF (.NOT.(IMN.EQ.IMX)) GO TO 10115
            IAM(IP1+9)=IMN
          GO TO 10113
10115     CONTINUE
            IAM(IP1+9)=0
10113     CONTINUE
          IAM(IP2+8)=0
          IAM(IP2+9)=0
          IAM(IP2+7)=0
        GO TO 10116
10101   CONTINUE
          IF (IAM(IP1+7).GT.0) IAM(IP2+7)=-ABS(IAM(IP2+7))
10116   CONTINUE
      GO TO L10086 , (10099,10097,10092,10090,10085)
10100 CONTINUE
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10117
        CALL ARDBPA (IAM,IDB,'AFTER REMOVING COINCIDENT SEGMENTS')
10117 CONTINUE
C
C
C Look for unclosed edges, if that is to be done.
C
      IF (.NOT.(KF2.EQ.0)) GO TO 10118
10119   CONTINUE
          NPF=0
          IPT=8
10120     CONTINUE
          IF (.NOT.(IAM(IPT+3).NE.18)) GO TO 10121
            IPT=IAM(IPT+3)
            IF (.NOT.(IAM(IPT+7).NE.0.AND.IAM(IAM(IPT+4)+7).EQ.0)) GO TO
     + 10122
              IGI=ABS(IAM(IPT+7))
              IP1=IAM(IPT+4)
              IP2=IP1
10123         CONTINUE
              IF (.NOT.(IAM(IAM(IP2+6)+1).EQ.IAM(IP1+1).AND.IAM(IAM(IP2+
     +6)+2).EQ.IAM(IP1+2))) GO TO 10124
                IP2=IAM(IP2+6)
              GO TO 10123
10124         CONTINUE
              IPF=0
10125         CONTINUE
                IF (.NOT.(IP2.NE.IP1.AND.(ABS(IAM(IP2+7)).EQ.IGI.OR.ABS(
     +IAM(IAM(IP2+3)+7)).EQ.IGI))) GO TO 10126
                  IPF=1
                  GO TO 10127
10126           CONTINUE
                IP2=IAM(IP2+5)
                IF (IAM(IP2+1).NE.IAM(IP1+1).OR.IAM(IP2+2).NE.IAM(IP1+2)
     +)         GO TO 10127
              GO TO 10125
10127         CONTINUE
              IF (.NOT.(IPF.EQ.0)) GO TO 10128
                IAM(IPT+7)=0
                NPF=NPF+1
10128         CONTINUE
10122       CONTINUE
          GO TO 10120
10121     CONTINUE
          IPT=18
10129     CONTINUE
          IF (.NOT.(IAM(IPT+4).NE.8)) GO TO 10130
            IPT=IAM(IPT+4)
            IF (.NOT.(IAM(IPT+7).NE.0.AND.IAM(IAM(IPT+3)+7).EQ.0)) GO TO
     + 10131
              IGI=ABS(IAM(IPT+7))
              IP1=IPT
              IP2=IP1
10132         CONTINUE
              IF (.NOT.(IAM(IAM(IP2+6)+1).EQ.IAM(IP1+1).AND.IAM(IAM(IP2+
     +6)+2).EQ.IAM(IP1+2))) GO TO 10133
                IP2=IAM(IP2+6)
              GO TO 10132
10133         CONTINUE
              IPF=0
10134         CONTINUE
                IF (.NOT.(IP2.NE.IP1.AND.(ABS(IAM(IP2+7)).EQ.IGI.OR.ABS(
     +IAM(IAM(IP2+3)+7)).EQ.IGI))) GO TO 10135
                  IPF=1
                  GO TO 10136
10135           CONTINUE
                IP2=IAM(IP2+5)
                IF (IAM(IP2+1).NE.IAM(IP1+1).OR.IAM(IP2+2).NE.IAM(IP1+2)
     +)         GO TO 10136
              GO TO 10134
10136         CONTINUE
              IF (.NOT.(IPF.EQ.0)) GO TO 10137
                IAM(IPT+7)=0
                NPF=NPF+1
10137         CONTINUE
10131       CONTINUE
          GO TO 10129
10130     CONTINUE
        IF (.NOT.(NPF.EQ.0)) GO TO 10119
C
C If debugging is turned on, produce a plot.
C
        IF (.NOT.(IDB.NE.0)) GO TO 10138
          CALL ARDBPA (IAM,IDB,'AFTER REMOVING UNCLOSED EDGES')
10138   CONTINUE
C
C
10118 CONTINUE
C
C Adjust the area identifiers for all edge segments in the map.  We
C first make a pass over the entire area map, looking for holes and
C eliminating them by the insertion of some temporary connecting lines.
C
C Save the pointer to the last word of the last node, so that we can
C remove the nodes implementing the temporary connecting lines before
C returning to the caller.
C
      ILW=IAM(5)
C
C Each pass through the following loop traces the boundary of one
C connected loop.  In some cases (for contour maps, for example),
C this step can be omitted; in those cases, there is a small chance
C of failure, in which case we have to come back and force the step
C to occur.
C
  104 CONTINUE
      IF (.NOT.(KF3.EQ.0)) GO TO 10139
C
C
        IPT=8
C
10140   CONTINUE
C
C Move to the right across the area map, looking for an edge segment
C that has not yet been completely processed.  If no such segment can
C be found, all subareas have been done.
C
10141     CONTINUE
          IF (.NOT.(MOD(IAM(IPT),4).EQ.3.OR.ABS(IAM(IPT+7)).LT.IAM(6)))
     +    GO TO 10142
            IPT=IAM(IPT+5)
            IF (IPT.EQ.18) GO TO 10143
          GO TO 10141
10142     CONTINUE
C
C Pull out the group identifier for the segment.
C
          IGI=ABS(IAM(IPT+7))
C
C Decide whether to scan the subarea to the left of the edge being
C traced (IPU=1) or the one to the right (IPU=2).
C
          IF (.NOT.(MOD(IAM(IPT),2).EQ.0)) GO TO 10144
            IPU=1
          GO TO 10145
10144     CONTINUE
            IPU=2
10145     CONTINUE
C
C IPQ points to the node defining the beginning of the edge segment,
C IPR to the node defining the end of the edge segment, and IPS to
C the node defining the beginning of the edge, so that we can tell
C when we've gone all the way around it.
C
          IPQ=IAM(IPT+4)
          IPR=IPT
          IPS=IPQ
          IPM=IPR
          IPV=IPU
C
C We need to keep track of the highest point found along the loop and
C the total change in direction.  Initialize the variables involved.
C
          IPH=IPQ
          ANT=0.
C
C Each pass through the following loop moves one step along the edge of
C the subarea.
C
10146     CONTINUE
C
C Update the pointer to the highest point found along the loop.
C
            IF (IAM(IPR+2).GT.IAM(IPH+2)) IPH=IPR
C
C Move IPQ to IPP and IPR to IPQ.  They point to the nodes defining the
C ends of the current edge segment.
C
            IPP=IPQ
            IPQ=IPR
C
C Get the coordinates of the ends of the edge segment for use in
C computing change in direction to a possible next point.
C
            IXP=IAM(IPP+1)
            IYP=IAM(IPP+2)
            IXQ=IAM(IPQ+1)
            IYQ=IAM(IPQ+2)
            FXP=REAL(IXP)
            FYP=REAL(IYP)
            FXQ=REAL(IXQ)
            FYQ=REAL(IYQ)
C
C Back up IPR to the beginning of the group of nodes which have the
C same x and y coordinates as it does.
C
10147       CONTINUE
            IF (.NOT.(IAM(IPR+1).EQ.IAM(IAM(IPR+6)+1).AND.IAM(IPR+2).EQ.
     +IAM(IAM(IPR+6)+2))) GO TO 10148
              IPR=IAM(IPR+6)
            GO TO 10147
10148       CONTINUE
C
C Go through the group of nodes, examining all the possible ways to
C move from the current position to a new one.  Pick the direction
C which is leftmost (if IPU=1) or rightmost (if IPU=2).
C
            IP1=IPR
            IP2=IPR
            IPR=0
            IF (.NOT.(IPU.EQ.1)) GO TO 10149
              ANM=-3.14159265358979
            GO TO 10150
10149       CONTINUE
              ANM=+3.14159265358979
10150       CONTINUE
C
10151       CONTINUE
            IF (.NOT.(IAM(IP2+1).EQ.IAM(IP1+1).AND.IAM(IP2+2).EQ.IAM(IP1
     ++2))) GO TO 10152
              IF (.NOT.(ABS(IAM(IAM(IP2+3)+7)).EQ.IGI.AND.(IAM(IAM(IP2+3
     +)+1).NE.IAM(IPP+1).OR.IAM(IAM(IP2+3)+2).NE.IAM(IPP+2)))) GO TO 101
     +53
                IXR=IAM(IAM(IP2+3)+1)
                IYR=IAM(IAM(IP2+3)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10154
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10155
10154           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10156
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10155
10156           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1)
                  CALL ARMPIA (IO7,DP2)
                  ANG=ARDAT2(DP1,DP2)
10155           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10157
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10158
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=1
10158             CONTINUE
                GO TO 10159
10157           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10160
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=2
10160             CONTINUE
10159           CONTINUE
10153         CONTINUE
              IF (.NOT.(ABS(IAM(IP2+7)).EQ.IGI.AND.(IAM(IAM(IP2+4)+1).NE
     +.IAM(IPP+1).OR.IAM(IAM(IP2+4)+2).NE.IAM(IPP+2)))) GO TO 10161
                IXR=IAM(IAM(IP2+4)+1)
                IYR=IAM(IAM(IP2+4)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10162
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10163
10162           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10164
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10163
10164           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1)
                  CALL ARMPIA (IO7,DP2)
                  ANG=ARDAT2(DP1,DP2)
10163           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10165
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10166
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=2
10166             CONTINUE
                GO TO 10167
10165           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10168
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=1
10168             CONTINUE
10167           CONTINUE
10161         CONTINUE
              IP2=IAM(IP2+5)
            GO TO 10151
10152       CONTINUE
C
C If no possible exit was found, reverse direction.
C
            IF (.NOT.(IPR.EQ.0)) GO TO 10169
              IPR=IPP
              IPV=3-IPV
              IF (.NOT.(IPU.EQ.1)) GO TO 10170
                ANM=+3.14159265358979
              GO TO 10171
10170         CONTINUE
                ANM=-3.14159265358979
10171         CONTINUE
10169       CONTINUE
C
C Update the total angular change.
C
            ANT=ANT+ANM
C
C Set the marker for the edge segment picked.  If the marker is set
C already, either go back and do a slow-path intersection search or
C log an error.
C
            IF (.NOT.(IPV.EQ.1.AND.MOD(IAM(IPM),2).EQ.0)) GO TO 10172
              IAM(IPM)=IAM(IPM)+1
            GO TO 10173
10172       CONTINUE
            IF (.NOT.(IPV.EQ.2.AND.MOD(IAM(IPM)/2,2).EQ.0)) GO TO 10174
              IAM(IPM)=IAM(IPM)+2
            GO TO 10173
10174       CONTINUE
              IPT=IAM(5)-9
10175         CONTINUE
              IF (.NOT.(IPT.GT.ILW)) GO TO 10176
                IAM(IAM(IPT+4)+3)=IAM(IPT+3)
                IAM(IAM(IPT+3)+4)=IAM(IPT+4)
                IAM(IAM(IPT+6)+5)=IAM(IPT+5)
                IAM(IAM(IPT+5)+6)=IAM(IPT+6)
                IPT=IPT-10
              GO TO 10175
10176         CONTINUE
              IAM(5)=ILW
              DO 10177 IPT=8,IAM(5)-9,10
                IAM(IPT)=4*(IAM(IPT)/4)
10177         CONTINUE
              IF (.NOT.(KF1.NE.0)) GO TO 10178
                KF1=0
                GO TO 101
10178         CONTINUE
                CALL SETER ('ARPRAM - ALGORITHM FAILURE',4,1)
                RETURN
10173       CONTINUE
C
C Exit if we're passing the start of the subarea.
C
            IF (IAM(IPQ+1).EQ.IAM(IPS+1).AND.IAM(IPQ+2).EQ.IAM(IPS+2).AN
     +D.IAM(IPR+1).EQ.IAM(IPT+1).AND.IAM(IPR+2).EQ.IAM(IPT+2)) GO TO 101
     +79
C
          GO TO 10146
10179     CONTINUE
C
C If the closed loop just traced was a hole, insert a temporary
C connecting line to get rid of the hole.
C
          IF (.NOT.((IPU.EQ.1.AND.ANT.LT.0.).OR.(IPU.EQ.2.AND.ANT.GT.0.)
     +))  GO TO 10180
            IOF=0
            XCI=REAL(IAM(IPH+1))
            YCI=REAL(IAM(IPH+2))
            YCO=RLP
            IP1=IPH
10181       CONTINUE
            IF (.NOT.(IAM(IAM(IP1+5)+1).EQ.IAM(IPH+1))) GO TO 10182
              IP1=IAM(IP1+5)
            GO TO 10181
10182       CONTINUE
10183       CONTINUE
            IF (.NOT.(IAM(IP1+1).GE.IAM(IPH+1)-IAM(2))) GO TO 10184
              IF (.NOT.(ABS(IAM(IP1+7)).EQ.IGI.AND.IAM(IAM(IP1+4)+1).GT.
     +IAM(IP1+1).AND.IAM(IAM(IP1+4)+1).GE.IAM(IPH+1))) GO TO 10185
                IF (.NOT.(IAU.EQ.1)) GO TO 10186
                  YTM=REAL(IAM(IP1+2))+
     +            (XCI-REAL(IAM(IP1+1)))*
     +         (REAL(IAM(IAM(IP1+4)+2)-IAM(IP1+2))/
     + REAL(IAM(IAM(IP1+4)+1)-IAM(IP1+1)))
                GO TO 10187
10186           CONTINUE
                  YTM=REAL(DBLE(IAM(IP1+2))+
     +            (DBLE(XCI)-DBLE(IAM(IP1+1)))*
     +         (DBLE(IAM(IAM(IP1+4)+2)-IAM(IP1+2))/
     + DBLE(IAM(IAM(IP1+4)+1)-IAM(IP1+1))))
10187           CONTINUE
                IF (.NOT.(YTM.GT.YCI.AND.YTM.LT.YCO)) GO TO 10188
                  IOF=IP1
                  YCO=YTM
10188           CONTINUE
10185         CONTINUE
              IF (.NOT.(ABS(IAM(IAM(IP1+3)+7)).EQ.IGI.AND.IAM(IAM(IP1+3)
     ++1).GT.IAM(IP1+1).AND.IAM(IAM(IP1+3)+1).GE.IAM(IPH+1))) GO TO 1018
     +9
                IF (.NOT.(IAU.EQ.1)) GO TO 10190
                  YTM=REAL(IAM(IP1+2))+
     +            (XCI-REAL(IAM(IP1+1)))*
     +         (REAL(IAM(IAM(IP1+3)+2)-IAM(IP1+2))/
     + REAL(IAM(IAM(IP1+3)+1)-IAM(IP1+1)))
                GO TO 10191
10190           CONTINUE
                  YTM=REAL(DBLE(IAM(IP1+2))+
     +            (DBLE(XCI)-DBLE(IAM(IP1+1)))*
     +         (DBLE(IAM(IAM(IP1+3)+2)-IAM(IP1+2))/
     + DBLE(IAM(IAM(IP1+3)+1)-IAM(IP1+1))))
10191           CONTINUE
                IF (.NOT.(YTM.GT.YCI.AND.YTM.LT.YCO)) GO TO 10192
                  IOF=IAM(IP1+3)
                  YCO=YTM
10192           CONTINUE
10189         CONTINUE
              IP1=IAM(IP1+6)
            GO TO 10183
10184       CONTINUE
            IF (.NOT.(IOF.NE.0)) GO TO 10193
              IX0=IAM(IPH+1)
              IY0=IAM(IPH+2)
              IF (.NOT.(INT(YCO).NE.IY0)) GO TO 10194
                IPI=18
                ASSIGN 10195 TO L10015
                GO TO 10015
10195           CONTINUE
                IAM(IPN+7)=0
                IAM(IPN+8)=0
                IAM(IPN+9)=0
                IY0=INT(YCO)
                ASSIGN 10196 TO L10015
                GO TO 10015
10196           CONTINUE
                IAM(IPN+7)=LAM-IGI
                IAM(IPN+8)=0
                IAM(IPN+9)=0
10194         CONTINUE
              IF (.NOT.((IX0.NE.IAM(IOF+1).OR.IY0.NE.IAM(IOF+2)).AND.(IX
     +0.NE.IAM(IAM(IOF+4)+1).OR.IY0.NE.IAM(IAM(IOF+4)+2)))) GO TO 10197
                IPI=IOF
                ASSIGN 10198 TO L10015
                GO TO 10015
10198           CONTINUE
10197         CONTINUE
10193       CONTINUE
C
10180     CONTINUE
C
        GO TO 10140
10143   CONTINUE
C
C Zero the lower bits in the markers in all the nodes.
C
        DO 10199 IPT=8,IAM(5)-9,10
          IAM(IPT)=4*(IAM(IPT)/4)
10199   CONTINUE
C
C If debugging is turned on, produce a plot.
C
        IF (.NOT.(IDB.NE.0)) GO TO 10200
          CALL ARDBPA (IAM,IDB,'AFTER LOOKING FOR HOLES')
10200   CONTINUE
C
C
10139 CONTINUE
C
C Now, make another pass through the area map, tracing one subarea at a
C time and setting the area identifiers in each.
C
C Each pass through the following loop traces the boundary of one
C subarea.
C
C
      IPT=8
C
10201 CONTINUE
C
C Move to the right across the area map, looking for an edge segment
C that has not yet been completely processed.  If no such segment can
C be found, all subareas have been done.
C
10202   CONTINUE
        IF (.NOT.(MOD(IAM(IPT),4).EQ.3.OR.ABS(IAM(IPT+7)).LT.IAM(6).OR.(
     +KF3.NE.0.AND.((MOD(IAM(IPT),2).NE.0.OR.IAM(IPT+8).GT.0).AND.(MOD(I
     +AM(IPT)/2,2).NE.0.OR.IAM(IPT+9).GT.0))))) GO TO 10203
          IPT=IAM(IPT+5)
          IF (IPT.EQ.18) GO TO 10204
        GO TO 10202
10203   CONTINUE
C
C Pull out the group identifier for the segment.
C
        IGI=ABS(IAM(IPT+7))
C
C Decide whether to scan the subarea to the left of the edge being
C traced (IPU=1) or the one to the right (IPU=2) and initialize the
C area identifier.
C
        IF (.NOT.(MOD(IAM(IPT),2).EQ.0.AND.(KF3.EQ.0.OR.IAM(IPT+8).LE.0)
     +))GO TO 10205
          IPU=1
        GO TO 10206
10205   CONTINUE
          IPU=2
10206   CONTINUE
C
        IAX=0
        IAI=0
        IAP=0
C
C IPQ points to the node defining the beginning of the edge segment,
C IPR to the node defining the end of the edge segment, and IPS to
C the node defining the beginning of the edge, so that we can tell
C when we've gone all the way around it.
C
        IPQ=IAM(IPT+4)
        IPR=IPT
        IPS=IPQ
        IPM=IPR
        IPV=IPU
C
C Each pass through the following loop moves one step along the edge of
C the subarea.
C
10207   CONTINUE
C
C Move IPQ to IPP and IPR to IPQ.
C
          IPP=IPQ
          IPQ=IPR
C
C Get the coordinates of the ends of the edge segment for use in
C computing change in direction to a possible next point.
C
          IXP=IAM(IPP+1)
          IYP=IAM(IPP+2)
          IXQ=IAM(IPQ+1)
          IYQ=IAM(IPQ+2)
          FXP=REAL(IXP)
          FYP=REAL(IYP)
          FXQ=REAL(IXQ)
          FYQ=REAL(IYQ)
C
C Back up IPR to the beginning of the group of nodes which have the
C same x and y coordinates as it does.
C
10208     CONTINUE
          IF (.NOT.(IAM(IPR+1).EQ.IAM(IAM(IPR+6)+1).AND.IAM(IPR+2).EQ.IA
     +M(IAM(IPR+6)+2))) GO TO 10209
            IPR=IAM(IPR+6)
          GO TO 10208
10209     CONTINUE
C
C If there is only one node in the group, the exit path is obvious.
C
          IF (.NOT.(IAM(IPR+1).NE.IAM(IAM(IPR+5)+1).OR.IAM(IPR+2).NE.IAM
     +(IAM(IPR+5)+2))) GO TO 10210
            IF (.NOT.(IAM(IAM(IPR+3)+1).NE.IAM(IPP+1).OR.IAM(IAM(IPR+3)+
     +2).NE.IAM(IPP+2))) GO TO 10211
              IF (.NOT.(IAM(IAM(IPR+3)+7).EQ.LAM-IGI.OR.ABS(IAM(IAM(IPR+
     +3)+7)).EQ.IGI)) GO TO 10212
                IPM=IAM(IPR+3)
                IPR=IPM
                IPV=IPU
              GO TO 10213
10212         CONTINUE
                IPR=0
10213         CONTINUE
            GO TO 10214
10211       CONTINUE
              IF (.NOT.(IAM(IPR+7).EQ.LAM-IGI.OR.ABS(IAM(IPR+7)).EQ.IGI)
     +)       GO TO 10215
                IPM=IPR
                IPR=IAM(IPR+4)
                IPV=3-IPU
              GO TO 10216
10215         CONTINUE
                IPR=0
10216         CONTINUE
10214       CONTINUE
C
C Otherwise, go through the group of nodes, examining all the possible
C ways to move from the current position to a new one.  Pick the
C direction which is leftmost (if IPU=1) or rightmost (if IPU=2).
C
          GO TO 10217
10210     CONTINUE
C
            IP1=IPR
            IP2=IPR
            IPR=0
            IF (.NOT.(IPU.EQ.1)) GO TO 10218
              ANM=-3.14159265358979
            GO TO 10219
10218       CONTINUE
              ANM=+3.14159265358979
10219       CONTINUE
C
10220       CONTINUE
            IF (.NOT.(IAM(IP2+1).EQ.IAM(IP1+1).AND.IAM(IP2+2).EQ.IAM(IP1
     ++2))) GO TO 10221
              IF (.NOT.((IAM(IAM(IP2+3)+7).EQ.LAM-IGI.OR.ABS(IAM(IAM(IP2
     ++3)+7)).EQ.IGI).AND.(IAM(IAM(IP2+3)+1).NE.IAM(IPP+1).OR.IAM(IAM(IP
     +2+3)+2).NE.IAM(IPP+2)))) GO TO 10222
                IXR=IAM(IAM(IP2+3)+1)
                IYR=IAM(IAM(IP2+3)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10223
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10224
10223           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10225
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10224
10225           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1)
                  CALL ARMPIA (IO7,DP2)
                  ANG=ARDAT2(DP1,DP2)
10224           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10226
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10227
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=1
10227             CONTINUE
                GO TO 10228
10226           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10229
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=2
10229             CONTINUE
10228           CONTINUE
10222         CONTINUE
              IF (.NOT.((IAM(IP2+7).EQ.LAM-IGI.OR.ABS(IAM(IP2+7)).EQ.IGI
     +).AND.(IAM(IAM(IP2+4)+1).NE.IAM(IPP+1).OR.IAM(IAM(IP2+4)+2).NE.IAM
     +(IPP+2)))) GO TO 10230
                IXR=IAM(IAM(IP2+4)+1)
                IYR=IAM(IAM(IP2+4)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10231
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10232
10231           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10233
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10232
10233           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1)
                  CALL ARMPIA (IO7,DP2)
                  ANG=ARDAT2(DP1,DP2)
10232           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10234
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10235
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=2
10235             CONTINUE
                GO TO 10236
10234           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10237
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=1
10237             CONTINUE
10236           CONTINUE
10230         CONTINUE
              IP2=IAM(IP2+5)
            GO TO 10220
10221       CONTINUE
C
10217     CONTINUE
C
C If no possible exit was found, reverse direction.
C
          IF (.NOT.(IPR.EQ.0)) GO TO 10238
            IPR=IPP
            IPV=3-IPV
10238     CONTINUE
C
C Update the markers for the edge segment picked.
C
          IF (.NOT.(IPV.EQ.1.AND.MOD(IAM(IPM),2).EQ.0)) GO TO 10239
            IAM(IPM)=IAM(IPM)+1
            IAQ=IPM+8
          GO TO 10240
10239     CONTINUE
          IF (.NOT.(IPV.EQ.2.AND.MOD(IAM(IPM)/2,2).EQ.0)) GO TO 10241
            IAM(IPM)=IAM(IPM)+2
            IAQ=IPM+9
          GO TO 10240
10241     CONTINUE
            CALL SETER ('ARPRAM - ALGORITHM FAILURE',5,1)
            RETURN
10240     CONTINUE
C
          IF (.NOT.(IAM(IAQ).LE.0.OR.IAM(IAQ).GE.IAM(6))) GO TO 10242
            IF (.NOT.(IAM(IAQ).LT.0)) GO TO 10243
              IAI=-1
              IAX=LAM*4
            GO TO 10244
10243       CONTINUE
            IF (.NOT.(IAM(IAQ).GE.IAM(6).AND.IAM(IPM).GT.IAX)) GO TO 102
     +45
              IAI=IAM(IAQ)
              IAX=IAM(IPM)
10244       CONTINUE
10245       CONTINUE
            IAM(IAQ)=IAP
            IAP=IAQ
10242     CONTINUE
C
C Exit if we're passing the start of the subarea.
C
          IF (IAM(IPQ+1).EQ.IAM(IPS+1).AND.IAM(IPQ+2).EQ.IAM(IPS+2).AND.
     +IAM(IPR+1).EQ.IAM(IPT+1).AND.IAM(IPR+2).EQ.IAM(IPT+2)) GO TO 10246
C
        GO TO 10207
10246   CONTINUE
C
C Go through the chain of area identifiers, updating them.
C
10247   CONTINUE
        IF (.NOT.(IAP.NE.0)) GO TO 10248
          IAQ=IAM(IAP)
          IAM(IAP)=IAI
          IAP=IAQ
        GO TO 10247
10248   CONTINUE
C
C If a zero identifier was selected for the area, see if the search for
C holes was suppressed and, if so, re-do it.
C
        IF (.NOT.(IAI.EQ.0.AND.KF3.NE.0)) GO TO 10249
          DO 10250 IPT=8,IAM(5)-9,10
            IAM(IPT)=4*(IAM(IPT)/4)
10250     CONTINUE
          KF3=0
          GO TO 104
10249   CONTINUE
C
      GO TO 10201
10204 CONTINUE
C
C Delete the nodes used to put in the temporary connecting lines.
C
      IPT=IAM(5)-9
10251 CONTINUE
      IF (.NOT.(IPT.GT.ILW)) GO TO 10252
        IAM(IAM(IPT+4)+3)=IAM(IPT+3)
        IAM(IAM(IPT+3)+4)=IAM(IPT+4)
        IAM(IAM(IPT+6)+5)=IAM(IPT+5)
        IAM(IAM(IPT+5)+6)=IAM(IPT+6)
        IPT=IPT-10
      GO TO 10251
10252 CONTINUE
      IAM(5)=ILW
C
C Zero the markers in all the remaining nodes.
C
      DO 10253 IPT=8,IAM(5)-9,10
        IAM(IPT)=0
10253 CONTINUE
C
C Update the map state.
C
      IAM(4)=1
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10254
        CALL ARDBPA (IAM,IDB,'AFTER UPDATING AREA IDENTIFIERS')
10254 CONTINUE
C
C
C Done.
C
      RETURN
C
C This internal procedure adds a new point in the existing part of the
C area map.  Note that ADD-A-POINT puts a new point which has the same
C coordinates as an old point after the old point in the area map (in
C coordinate order); this is important.
C
10015 CONTINUE
        IPN=IAM(5)+1
        IF (.NOT.(IAM(5)+10.GE.IAM(6))) GO TO 10255
          CALL SETER ('ARPRAM - AREA-MAP ARRAY OVERFLOW',6,1)
          RETURN
10255   CONTINUE
        IAM(5)=IAM(5)+10
        IAM(IPN)=IAM(IPI)
        IAM(IPN+1)=IX0
        IAM(IPN+2)=IY0
        IAM(IPN+3)=IPI
        IAM(IPN+4)=IAM(IPI+4)
        IAM(IAM(IPI+4)+3)=IPN
        IAM(IPI+4)=IPN
10256   CONTINUE
          IF (.NOT.(IAM(IPN+1).LT.IAM(IPX+1))) GO TO 10257
            IPX=IAM(IPX+6)
          GO TO 10258
10257     CONTINUE
          IF (.NOT.(IAM(IPN+1).GT.IAM(IAM(IPX+5)+1))) GO TO 10259
            IPX=IAM(IPX+5)
          GO TO 10258
10259     CONTINUE
10260       CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IPX+1).AND.IAM(IPN+2).LT.IAM(I
     +PX+2))) GO TO 10261
                IPX=IAM(IPX+6)
              GO TO 10262
10261         CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IAM(IPX+5)+1).AND.IAM(IPN+2).G
     +T.IAM(IAM(IPX+5)+2))) GO TO 10263
                IPX=IAM(IPX+5)
              GO TO 10262
10263         CONTINUE
10264           CONTINUE
                IF (.NOT.(IAM(IAM(IPX+5)+1).EQ.IAM(IPN+1).AND.IAM(IAM(IP
     +X+5)+2).EQ.IAM(IPN+2))) GO TO 10265
                  IPX=IAM(IPX+5)
                GO TO 10264
10265           CONTINUE
                GO TO 10266
10262         CONTINUE
            GO TO 10260
10266       CONTINUE
            GO TO 10267
10258     CONTINUE
        GO TO 10256
10267   CONTINUE
        IAM(IPN+5)=IAM(IPX+5)
        IAM(IPN+6)=IAM(IAM(IPX+5)+6)
        IAM(IAM(IPX+5)+6)=IPN
        IAM(IPX+5)=IPN
        IAM(IPN+7)=IAM(IPI+7)
        IAM(IPN+8)=IAM(IPI+8)
        IAM(IPN+9)=IAM(IPI+9)
      GO TO L10015 , (10198,10196,10195,10079,10071,10069,10025,10014)
C
      END
