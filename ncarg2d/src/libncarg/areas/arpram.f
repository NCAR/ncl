C
C $Id: arpram.f,v 1.8 1994-03-16 23:11:35 kennison Exp $
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
     +                IDC,IDI,RLA,RWA,RDI,RSI
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
C Declare a temporary variable to hold error messages and the SETER
C function that returns the current error message.
C
      CHARACTER*80 MSG
C
      CHARACTER*113 SEMESS
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
C Check for an uncleared prior error.
C
      IF (ICFELL('ARPRAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pull out the length of the area map and check for initialization.
C
      LAM=IAM(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAM(LAM).NE.LAM)) GO TO 10001
        CALL SETER ('ARPRAM - INITIALIZATION DONE IMPROPERLY',2,1)
        RETURN
10001 CONTINUE
C
C Save the pointer to the last word of the last node, so that, in case
C of error, we can remove new nodes added to the area map.  Also, zero
C the variable that saves the upper-end pointer; again, this is done so
C that, when an error occurs, the proper action can be taken to dispose
C of any space temporarily used at the upper end of the area map array.
C
      ILW=IAM(5)
      ISU=0
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
        IF (ICFELL('ARPRAM',3).NE.0) RETURN
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
        CALL SETER ('ARPRAM - NO EDGES IN AREA MAP',4,1)
        GO TO 10008
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
10009 CONTINUE
      IF (.NOT.(IAM(IPI+3).NE.18)) GO TO 10010
        IPI=IAM(IPI+3)
        IF (.NOT.(IAM(IPI+7).NE.0)) GO TO 10011
          NDO=1+(ABS(IAM(IPI+1)-IAM(IAM(IPI+4)+1))-1)/
     +                                              IAM(2)
          IF (.NOT.(NDO.GT.1)) GO TO 10012
            IXL=IAM(IAM(IPI+4)+1)
            IYL=IAM(IAM(IPI+4)+2)
            FXD=REAL(IAM(IPI+1)-IXL)/REAL(NDO)
            FYD=REAL(IAM(IPI+2)-IYL)/REAL(NDO)
              I = 1
              GO TO 10015
10013         CONTINUE
              I =I +1
10015         CONTINUE
              IF (I .GT.(NDO-1)) GO TO 10014
              IX0=IXL+NINT(REAL(I)*FXD)
              IY0=IYL+NINT(REAL(I)*FYD)
              L10017=    1
              GO TO 10017
10016         CONTINUE
            GO TO 10013
10014       CONTINUE
10012     CONTINUE
10011   CONTINUE
      GO TO 10009
10010 CONTINUE
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10018
        CALL ARDBPA (IAM,IDB,'AFTER BREAKING UP LONG EDGE SEGMENTS')
        IF (ICFELL('ARPRAM',5).NE.0) RETURN
10018 CONTINUE
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
10019 CONTINUE
      IF (.NOT.(IAM(ISP+5).NE.18)) GO TO 10020
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
10021   CONTINUE
C
          IF (IQT.EQ.0) GO TO 10022
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
          IF (.NOT.(IAU.EQ.1)) GO TO 10023
            TMP=MAX(0.,MIN(1.,
     +          ((FSX-FX1)*(FX2-FX1)+(FSY-FY1)*(FY2-FY1))/
     +          ((FX2-FX1)*(FX2-FX1)+(FY2-FY1)*(FY2-FY1))))
            DSQ=(FX1-FSX+(FX2-FX1)*TMP)**2+(FY1-FSY+(FY2-FY1)*TMP)**2
          GO TO 10024
10023     CONTINUE
          IF (.NOT.(IAU.EQ.2)) GO TO 10025
            DPT=MAX(0.D0,MIN(1.D0,(DBLE(ISX-IX1)*DBLE(IX2-IX1)+
     +                             DBLE(ISY-IY1)*DBLE(IY2-IY1))/
     +                            (DBLE(IX2-IX1)*DBLE(IX2-IX1)+
     +                             DBLE(IY2-IY1)*DBLE(IY2-IY1))))
            DSQ=REAL((DBLE(IX1-ISX)+DBLE(IX2-IX1)*DPT)**2+
     +               (DBLE(IY1-ISY)+DBLE(IY2-IY1)*DPT)**2)
          GO TO 10024
10025     CONTINUE
            IO1(3,1)=IX2-IX1
            IO1(3,2)=IY2-IY1
            IO1(3,3)=ISX-IX1
            IO1(3,4)=ISY-IY1
            CALL ARMPIA (IO1,DP1,IER)
            IF (.NOT.(IER.NE.0)) GO TO 10026
              GO TO 10028
10026       CONTINUE
            CALL ARMPIA (IO2,DP2,IER)
            IF (.NOT.(IER.NE.0)) GO TO 10029
              GO TO 10028
10029       CONTINUE
            DPT=MAX(0.D0,MIN(1.D0,DP1/DP2))
            DSQ=REAL((DBLE(IX1-ISX)+DBLE(IX2-IX1)*DPT)**2+
     +               (DBLE(IY1-ISY)+DBLE(IY2-IY1)*DPT)**2)
10024     CONTINUE
C
          IF (.NOT.((DSQ.LT.1.).AND.(IX1.NE.ISX.OR.IY1.NE.ISY).AND.(IX2.
     +NE.ISX.OR.IY2.NE.ISY))) GO TO 10031
            IPI=IAM(IQT+4)
            IX0=ISX
            IY0=ISY
            L10017=    2
            GO TO 10017
10032       CONTINUE
            IAM(IQT+3)=IPN
            IF (IAM(IQT+4).NE.IAM(IQT+2)) IAM(IQT+4)=IPN
10031     CONTINUE
C
          IF (.NOT.(IAM(IAM(IQT+3)+1).EQ.ISX.AND.IAM(IAM(IQT+3)+2).EQ.IS
     +Y)) GO TO 10033
            IF (.NOT.(IAM(IQT+1).EQ.0)) GO TO 10034
              IQB=IAM(IQT)
              IF (IQB.NE.0) IAM(IQB+1)=0
              IAM(IQT)=IQE
              IQE=IQT
              IQT=IQB
            GO TO 10035
10034       CONTINUE
            IF (.NOT.(IAM(IQT).EQ.0)) GO TO 10036
              IAM(IAM(IQT+1))=0
              IAM(IQT)=IQE
              IQE=IQT
              IQT=0
            GO TO 10035
10036       CONTINUE
              IQL=IAM(IQT+1)
              IQU=IAM(IQT)
              IAM(IQL)=IQU
              IAM(IQU+1)=IQL
              IAM(IQT)=IQE
              IQE=IQT
              IF (.NOT.(IAM(IAM(IQU+3)+1).NE.ISX.OR.IAM(IAM(IQU+3)+2).NE
     +.ISY))  GO TO 10037
                L10039=    1
                GO TO 10039
10038           CONTINUE
10037         CONTINUE
              IQT=IQU
10035       CONTINUE
          GO TO 10040
10033     CONTINUE
            IQT=IAM(IQT)
10040     CONTINUE
C
        GO TO 10021
10022   CONTINUE
C
C Add line segments having the current point as a left endpoint.
C
        IF (.NOT.(IAM(ISP+7).NE.0.AND.(IAM(ISP+1).LT.IAM(IAM(ISP+4)+1).O
     +R.(IAM(ISP+1).EQ.IAM(IAM(ISP+4)+1).AND.IAM(ISP+2).LT.IAM(IAM(ISP+4
     +)+2))))) GO TO 10041
          ISL=ISP
          ISR=IAM(ISP+4)
          ISD=ISP
          L10043=    1
          GO TO 10043
10042     CONTINUE
10041   CONTINUE
C
        IF (.NOT.(IAM(IAM(ISP+3)+7).NE.0.AND.(IAM(ISP+1).LT.IAM(IAM(ISP+
     +3)+1).OR.(IAM(ISP+1).EQ.IAM(IAM(ISP+3)+1).AND.IAM(ISP+2).LT.IAM(IA
     +M(ISP+3)+2))))) GO TO 10044
          ISL=ISP
          ISR=IAM(ISP+3)
          ISD=ISR
          L10043=    2
          GO TO 10043
10045     CONTINUE
10044   CONTINUE
C
      GO TO 10019
10020 CONTINUE
C
C The following internal procedure adds a segment to the list.  It also
C checks for intersection of that segment with the segments on either
C side of it in the list.
C
      GO TO 10046
10043 CONTINUE
C
        IQS=0
        IQT=IQB
        ILX=IAM(ISL+1)
        RLY=REAL(IAM(ISL+2))
C
        IF (.NOT.(IAM(ISL+1).NE.IAM(ISR+1))) GO TO 10047
          SLL=REAL(IAM(ISR+2)-IAM(ISL+2))/
     +  REAL(IAM(ISR+1)-IAM(ISL+1))
        GO TO 10048
10047   CONTINUE
          SLL=RLP
10048   CONTINUE
C
10049   CONTINUE
          IF (IQT.EQ.0) GO TO 10050
          IF (.NOT.(IAM(IAM(IQT+2)+1).NE.IAM(IAM(IQT+3)+1))) GO TO 10051
            IF (.NOT.(IAU.EQ.1)) GO TO 10052
              SLP=REAL(IAM(IAM(IQT+3)+2)-IAM(IAM(IQT+2)+2))/
     +      REAL(IAM(IAM(IQT+3)+1)-IAM(IAM(IQT+2)+1))
              RTY=REAL(IAM(IAM(IQT+2)+2))+
     +         REAL(ILX-IAM(IAM(IQT+2)+1))*SLP
            GO TO 10053
10052       CONTINUE
              DPT=DBLE(IAM(IAM(IQT+3)+2)-IAM(IAM(IQT+2)+2))/
     +      DBLE(IAM(IAM(IQT+3)+1)-IAM(IAM(IQT+2)+1))
              SLP=REAL(DPT)
              RTY=DBLE(IAM(IAM(IQT+2)+2))+
     +         DBLE(ILX-IAM(IAM(IQT+2)+1))*DPT
10053       CONTINUE
          GO TO 10054
10051     CONTINUE
            SLP=RLP
            RTY=REAL(IAM(IAM(IQT+2)+2))
10054     CONTINUE
          IF (RLY.LT.RTY.OR.(RLY.EQ.RTY.AND.SLL.LT.SLP)) GO TO 10050
          IQS=IQT
          IQT=IAM(IQT)
        GO TO 10049
10050   CONTINUE
C
        IF (.NOT.(IQE.NE.0)) GO TO 10055
          IQI=IQE
          IQE=IAM(IQE)
        GO TO 10056
10055   CONTINUE
          IF (.NOT.(IAM(6)-5.LE.IAM(5))) GO TO 10057
            CALL SETER ('ARPRAM - AREA-MAP ARRAY OVERFLOW',6,1)
            GO TO 10008
10057     CONTINUE
          IQI=IAM(6)-5
          IAM(6)=IQI
10056   CONTINUE
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
        IF (.NOT.(IQS.NE.0)) GO TO 10059
          IQL=IQS
          IQU=IQI
          L10039=    2
          GO TO 10039
10060     CONTINUE
10059   CONTINUE
C
        IF (.NOT.(IQT.NE.0)) GO TO 10061
          IQL=IQI
          IQU=IQT
          L10039=    3
          GO TO 10039
10062     CONTINUE
10061   CONTINUE
C
      GO TO (10042,10045) , L10043
10046 CONTINUE
C
C The following internal procedure looks for intersection between the
C segments identified by the pointers IQL and IQU.  When such points of
C intersection are found, they are interpolated along the intersecting
C line segments and any line segments which pass within one unit.
C
      GO TO 10063
10039 CONTINUE
C
        IF (.NOT.(KF1.EQ.0.OR.IAM(IAM(IQL+4)+8).LE.0.OR.IAM(IAM(IQL+4)+9
     +).LE.0.OR.IAM(IAM(IQU+4)+8).LE.0.OR.IAM(IAM(IQU+4)+9).LE.0))
     +  GO TO 10064
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
          IF (.NOT.(IAU.EQ.1)) GO TO 10065
            TMP=(FX2-FX1)*(FY4-FY3)-(FX4-FX3)*(FY2-FY1)
          GO TO 10066
10065     CONTINUE
          IF (.NOT.(IAU.EQ.2)) GO TO 10067
            DPT=DBLE(IX2-IX1)*DBLE(IY4-IY3)-
     +          DBLE(IX4-IX3)*DBLE(IY2-IY1)
            TMP=REAL(DPT)
          GO TO 10066
10067     CONTINUE
            IO3(3, 1)=IX2-IX1
            IO3(3, 2)=IY4-IY3
            IO3(3, 3)=IX4-IX3
            IO3(3, 4)=IY2-IY1
            CALL ARMPIA (IO3,DPT,IER)
            IF (.NOT.(IER.NE.0)) GO TO 10068
              GO TO 10028
10068       CONTINUE
            TMP=REAL(DPT)
10066     CONTINUE
C
          IF (.NOT.(ABS(TMP).GT..1)) GO TO 10070
C
            IF (.NOT.(IAU.EQ.1)) GO TO 10071
              IX0=INT(MAX(-1.,MIN(RLP,
     +                ((FX4-FX3)*(FX2*FY1-FX1*FY2)-
     +                 (FX2-FX1)*(FX4*FY3-FX3*FY4))/TMP+.5)))
            GO TO 10072
10071       CONTINUE
            IF (.NOT.(IAU.EQ.2)) GO TO 10073
              IX0=INT(MAX(-1.D0,MIN(DLP,
     +                (DBLE(IX4-IX3)*
     +                (DBLE(IX2)*DBLE(IY1)-DBLE(IX1)*DBLE(IY2))-
     +                 DBLE(IX2-IX1)*
     +                (DBLE(IX4)*DBLE(IY3)-DBLE(IX3)*DBLE(IY4)))/
     +                                                     DPT+.5D0)))
            GO TO 10072
10073       CONTINUE
              IO4(3, 1)=IX2
              IO4(3, 2)=IY1
              IO4(3, 4)=IX1
              IO4(3, 5)=IY2
              IO4(3, 8)=IX4
              IO4(3, 9)=IY3
              IO4(3,11)=IX3
              IO4(3,12)=IY4
              CALL ARMPIA (IO4,DX0,IER)
              IF (.NOT.(IER.NE.0)) GO TO 10074
                GO TO 10028
10074         CONTINUE
              IX0=INT(MAX(-1.D0,MIN(DLP,DX0/DPT+.5D0)))
10072       CONTINUE
C
            IF (.NOT.(IAU.EQ.1)) GO TO 10076
              IY0=INT(MAX(-1.,MIN(RLP,
     +                ((FY4-FY3)*(FX2*FY1-FX1*FY2)-
     +                 (FY2-FY1)*(FX4*FY3-FX3*FY4))/TMP+.5)))
            GO TO 10077
10076       CONTINUE
            IF (.NOT.(IAU.EQ.2)) GO TO 10078
              IY0=INT(MAX(-1.D0,MIN(DLP,
     +                (DBLE(IY4-IY3)*
     +                (DBLE(IX2)*DBLE(IY1)-DBLE(IX1)*DBLE(IY2))-
     +                 DBLE(IY2-IY1)*
     +                (DBLE(IX4)*DBLE(IY3)-DBLE(IX3)*DBLE(IY4)))/
     +                                                     DPT+.5D0)))
            GO TO 10077
10078       CONTINUE
              CALL ARMPIA (IO5,DY0,IER)
              IF (.NOT.(IER.NE.0)) GO TO 10079
                GO TO 10028
10079         CONTINUE
              IY0=INT(MAX(-1.D0,MIN(DLP,DY0/DPT+.5D0)))
10077       CONTINUE
C
            IF (IX0.EQ.ISX.AND.IY0.LT.ISY) IX0=IX0+1
C
            FX0=REAL(IX0)
            FY0=REAL(IY0)
C
            IF (.NOT.((FX0-FX1)*(FX0-FX2).LE.0..AND.(FY0-FY1)*(FY0-FY2).
     +LE.0..AND.(FX0-FX3)*(FX0-FX4).LE.0..AND.(FY0-FY3)*(FY0-FY4).LE.0.)
     +)     GO TO 10081
C
              INP=0
C
              IF (.NOT.((IX0.NE.IX1.OR.IY0.NE.IY1).AND.(IX0.NE.IX2.OR.IY
     +0.NE.IY2))) GO TO 10082
                INP=1
                IPI=IAM(IQL+4)
                L10017=    3
                GO TO 10017
10083           CONTINUE
                IAM(IQL+3)=IPN
                IF (IAM(IQL+4).NE.IAM(IQL+2)) IAM(IQL+4)=IPN
10082         CONTINUE
C
              IF (.NOT.((IX0.NE.IX3.OR.IY0.NE.IY3).AND.(IX0.NE.IX4.OR.IY
     +0.NE.IY4))) GO TO 10084
                INP=1
                IPI=IAM(IQU+4)
                L10017=    4
                GO TO 10017
10085           CONTINUE
                IAM(IQU+3)=IPN
                IF (IAM(IQU+4).NE.IAM(IQU+2)) IAM(IQU+4)=IPN
10084         CONTINUE
C
              IF (.NOT.(INP.NE.0)) GO TO 10086
                IQT=IQB
10087           CONTINUE
                  IF (IQT.EQ.0) GO TO 10088
                  IX1=IAM(IAM(IQT+2)+1)
                  FX1=REAL(IX1)
                  IY1=IAM(IAM(IQT+2)+2)
                  FY1=REAL(IY1)
                  IX2=IAM(IAM(IQT+3)+1)
                  FX2=REAL(IX2)
                  IY2=IAM(IAM(IQT+3)+2)
                  FY2=REAL(IY2)
C
                  IF (.NOT.(IAU.EQ.1)) GO TO 10089
                    TMP=MAX(0.,MIN(1.,
     +                  ((FX0-FX1)*(FX2-FX1)+(FY0-FY1)*(FY2-FY1))/
     +                  ((FX2-FX1)*(FX2-FX1)+(FY2-FY1)*(FY2-FY1))))
                    DSQ=(FX1-FX0+(FX2-FX1)*TMP)**2+
     +                  (FY1-FY0+(FY2-FY1)*TMP)**2
                  GO TO 10090
10089             CONTINUE
                  IF (.NOT.(IAU.EQ.2)) GO TO 10091
                    DPT=MAX(0.D0,MIN(1.D0,
     +                                 (DBLE(IX0-IX1)*DBLE(IX2-IX1)+
     +                                  DBLE(IY0-IY1)*DBLE(IY2-IY1))/
     +                                 (DBLE(IX2-IX1)*DBLE(IX2-IX1)+
     +                                  DBLE(IY2-IY1)*DBLE(IY2-IY1))))
                    DSQ=(DBLE(IX1-IX0)+DBLE(IX2-IX1)*DPT)**2+
     +                  (DBLE(IY1-IY0)+DBLE(IY2-IY1)*DPT)**2
                  GO TO 10090
10091             CONTINUE
                    IO1(3,1)=IX2-IX1
                    IO1(3,2)=IY2-IY1
                    IO1(3,3)=IX0-IX1
                    IO1(3,4)=IY0-IY1
                    CALL ARMPIA (IO1,DP1,IER)
                    IF (.NOT.(IER.NE.0)) GO TO 10092
                      GO TO 10028
10092               CONTINUE
                    CALL ARMPIA (IO2,DP2,IER)
                    IF (.NOT.(IER.NE.0)) GO TO 10094
                      GO TO 10028
10094               CONTINUE
                    DPT=MAX(0.D0,MIN(1.D0,DP1/DP2))
                    DSQ=(DBLE(IX1-IX0)+DBLE(IX2-IX1)*DPT)**2+
     +                  (DBLE(IY1-IY0)+DBLE(IY2-IY1)*DPT)**2
10090             CONTINUE
C
                  IF (.NOT.((DSQ.LT.1.).AND.(IX1.NE.IX0.OR.IY1.NE.IY0).A
     +ND.(IX2.NE.IX0.OR.IY2.NE.IY0))) GO TO 10096
                    IPI=IAM(IQT+4)
                    L10017=    5
                    GO TO 10017
10097               CONTINUE
                    IAM(IQT+3)=IPN
                    IF (IAM(IQT+4).NE.IAM(IQT+2)) IAM(IQT+4)=IPN
10096             CONTINUE
                  IQT=IAM(IQT)
                GO TO 10087
10088           CONTINUE
10086         CONTINUE
C
10081       CONTINUE
C
10070     CONTINUE
C
10064   CONTINUE
C
      GO TO (10038,10060,10062) , L10039
10063 CONTINUE
C
C Restore the index of the last element used at the upper end of the
C area map.
C
      IAM(6)=ISU
      ISU=0
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10098
        CALL ARDBPA (IAM,IDB,'AFTER FINDING POINTS OF INTERSECTION')
        IF (ICFELL('ARPRAM',7).NE.0) RETURN
10098 CONTINUE
C
C
C Now look for coincident line segments in the list and remove them.
C
      ISP=8
10099 CONTINUE
      IF (.NOT.(IAM(ISP+5).NE.18)) GO TO 10100
        ISP=IAM(ISP+5)
        IF (.NOT.(IAM(ISP+7).NE.0)) GO TO 10101
          IF (.NOT.(IAM(IAM(ISP+3)+7).NE.0.AND.IAM(IAM(ISP+3)+1).EQ.IAM(
     +IAM(ISP+4)+1).AND.IAM(IAM(ISP+3)+2).EQ.IAM(IAM(ISP+4)+2))) GO TO 1
     +0102
            IP1=ISP
            IP2=IAM(ISP+3)
            L10104=    1
            GO TO 10104
10103       CONTINUE
10102     CONTINUE
          ISQ=IAM(ISP+5)
10105     CONTINUE
          IF (.NOT.(IAM(ISQ+1).EQ.IAM(ISP+1).AND.IAM(ISQ+2).EQ.IAM(ISP+2
     +))) GO TO 10106
            IF (.NOT.(IAM(ISQ+7).NE.0.AND.IAM(IAM(ISQ+4)+1).EQ.IAM(IAM(I
     +SP+4)+1).AND.IAM(IAM(ISQ+4)+2).EQ.IAM(IAM(ISP+4)+2))) GO TO 10107
              IP1=ISP
              IP2=ISQ
              L10104=    2
              GO TO 10104
10108         CONTINUE
10107       CONTINUE
            IF (.NOT.(IAM(IAM(ISQ+3)+7).NE.0.AND.IAM(IAM(ISQ+3)+1).EQ.IA
     +M(IAM(ISP+4)+1).AND.IAM(IAM(ISQ+3)+2).EQ.IAM(IAM(ISP+4)+2))) GO TO
     + 10109
              IP1=ISP
              IP2=IAM(ISQ+3)
              L10104=    3
              GO TO 10104
10110         CONTINUE
10109       CONTINUE
            ISQ=IAM(ISQ+5)
          GO TO 10105
10106     CONTINUE
10101   CONTINUE
        IF (.NOT.(IAM(IAM(ISP+3)+7).NE.0)) GO TO 10111
          ISQ=IAM(ISP+5)
10112     CONTINUE
          IF (.NOT.(IAM(ISQ+1).EQ.IAM(ISP+1).AND.IAM(ISQ+2).EQ.IAM(ISP+2
     +))) GO TO 10113
            IF (.NOT.(IAM(ISQ+7).NE.0.AND.IAM(IAM(ISQ+4)+1).EQ.IAM(IAM(I
     +SP+3)+1).AND.IAM(IAM(ISQ+4)+2).EQ.IAM(IAM(ISP+3)+2))) GO TO 10114
              IP1=IAM(ISP+3)
              IP2=ISQ
              L10104=    4
              GO TO 10104
10115         CONTINUE
10114       CONTINUE
            IF (.NOT.(IAM(IAM(ISQ+3)+7).NE.0.AND.IAM(IAM(ISQ+3)+1).EQ.IA
     +M(IAM(ISP+3)+1).AND.IAM(IAM(ISQ+3)+2).EQ.IAM(IAM(ISP+3)+2))) GO TO
     + 10116
              IP1=IAM(ISP+3)
              IP2=IAM(ISQ+3)
              L10104=    5
              GO TO 10104
10117         CONTINUE
10116       CONTINUE
            ISQ=IAM(ISQ+5)
          GO TO 10112
10113     CONTINUE
10111   CONTINUE
      GO TO 10099
10100 CONTINUE
C
C This internal procedure processes coincident pairs of segments found.
C If both members of the pair belong to the same group, area identifier
C information from both members is reconciled and one of the pair is
C deleted.  If they belong to different groups, the group id in one of
C them is negated, so that it is present when we are looking at edges
C belonging to a single group, but absent when we are looking at all
C the edges together.
C
      GO TO 10118
10104 CONTINUE
        IF (.NOT.(ABS(IAM(IP1+7)).EQ.ABS(IAM(IP2+7)))) GO TO 10119
          IL1=IAM(IP1+8)
          IR1=IAM(IP1+9)
          IF (.NOT.(IAM(IP1+1).EQ.IAM(IP2+1).AND.IAM(IP1+2).EQ.IAM(IP2+2
     +))) GO TO 10120
            IL2=IAM(IP2+8)
            IR2=IAM(IP2+9)
          GO TO 10121
10120     CONTINUE
            IL2=IAM(IP2+9)
            IR2=IAM(IP2+8)
10121     CONTINUE
          IMN=MIN(IL1,IL2)
          IMX=MAX(IL1,IL2)
          IF (.NOT.(IMN.LT.0)) GO TO 10122
            IF (.NOT.(IMX.LT.0)) GO TO 10123
              IAM(IP1+8)=-1
            GO TO 10124
10123       CONTINUE
              IAM(IP1+8)=0
10124       CONTINUE
          GO TO 10125
10122     CONTINUE
          IF (.NOT.(IMN.EQ.0)) GO TO 10126
            IAM(IP1+8)=IMX
          GO TO 10125
10126     CONTINUE
          IF (.NOT.(IMN.EQ.IMX)) GO TO 10127
            IAM(IP1+8)=IMN
          GO TO 10125
10127     CONTINUE
            IAM(IP1+8)=0
10125     CONTINUE
          IMN=MIN(IR1,IR2)
          IMX=MAX(IR1,IR2)
          IF (.NOT.(IMN.LT.0)) GO TO 10128
            IF (.NOT.(IMX.LT.0)) GO TO 10129
              IAM(IP1+9)=-1
            GO TO 10130
10129       CONTINUE
              IAM(IP1+9)=0
10130       CONTINUE
          GO TO 10131
10128     CONTINUE
          IF (.NOT.(IMN.EQ.0)) GO TO 10132
            IAM(IP1+9)=IMX
          GO TO 10131
10132     CONTINUE
          IF (.NOT.(IMN.EQ.IMX)) GO TO 10133
            IAM(IP1+9)=IMN
          GO TO 10131
10133     CONTINUE
            IAM(IP1+9)=0
10131     CONTINUE
          IAM(IP2+8)=0
          IAM(IP2+9)=0
          IAM(IP2+7)=0
        GO TO 10134
10119   CONTINUE
          IF (IAM(IP1+7).GT.0) IAM(IP2+7)=-ABS(IAM(IP2+7))
10134   CONTINUE
      GO TO (10103,10108,10110,10115,10117) , L10104
10118 CONTINUE
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10135
        CALL ARDBPA (IAM,IDB,'AFTER REMOVING COINCIDENT SEGMENTS')
        IF (ICFELL('ARPRAM',8).NE.0) RETURN
10135 CONTINUE
C
C
C Look for unclosed edges, if that is to be done.
C
      IF (.NOT.(KF2.EQ.0)) GO TO 10136
10137   CONTINUE
          NPF=0
          IPT=8
10138     CONTINUE
          IF (.NOT.(IAM(IPT+3).NE.18)) GO TO 10139
            IPT=IAM(IPT+3)
            IF (.NOT.(IAM(IPT+7).NE.0.AND.IAM(IAM(IPT+4)+7).EQ.0)) GO TO
     + 10140
              IGI=ABS(IAM(IPT+7))
              IP1=IAM(IPT+4)
              IP2=IP1
10141         CONTINUE
              IF (.NOT.(IAM(IAM(IP2+6)+1).EQ.IAM(IP1+1).AND.IAM(IAM(IP2+
     +6)+2).EQ.IAM(IP1+2))) GO TO 10142
                IP2=IAM(IP2+6)
              GO TO 10141
10142         CONTINUE
              IPF=0
10143         CONTINUE
                IF (.NOT.(IP2.NE.IP1.AND.(ABS(IAM(IP2+7)).EQ.IGI.OR.ABS(
     +IAM(IAM(IP2+3)+7)).EQ.IGI))) GO TO 10144
                  IPF=1
                  GO TO 10145
10144           CONTINUE
                IP2=IAM(IP2+5)
                IF (IAM(IP2+1).NE.IAM(IP1+1).OR.IAM(IP2+2).NE.IAM(IP1+2)
     +)         GO TO 10145
              GO TO 10143
10145         CONTINUE
              IF (.NOT.(IPF.EQ.0)) GO TO 10146
                IAM(IPT+7)=0
                NPF=NPF+1
10146         CONTINUE
10140       CONTINUE
          GO TO 10138
10139     CONTINUE
          IPT=18
10147     CONTINUE
          IF (.NOT.(IAM(IPT+4).NE.8)) GO TO 10148
            IPT=IAM(IPT+4)
            IF (.NOT.(IAM(IPT+7).NE.0.AND.IAM(IAM(IPT+3)+7).EQ.0)) GO TO
     + 10149
              IGI=ABS(IAM(IPT+7))
              IP1=IPT
              IP2=IP1
10150         CONTINUE
              IF (.NOT.(IAM(IAM(IP2+6)+1).EQ.IAM(IP1+1).AND.IAM(IAM(IP2+
     +6)+2).EQ.IAM(IP1+2))) GO TO 10151
                IP2=IAM(IP2+6)
              GO TO 10150
10151         CONTINUE
              IPF=0
10152         CONTINUE
                IF (.NOT.(IP2.NE.IP1.AND.(ABS(IAM(IP2+7)).EQ.IGI.OR.ABS(
     +IAM(IAM(IP2+3)+7)).EQ.IGI))) GO TO 10153
                  IPF=1
                  GO TO 10154
10153           CONTINUE
                IP2=IAM(IP2+5)
                IF (IAM(IP2+1).NE.IAM(IP1+1).OR.IAM(IP2+2).NE.IAM(IP1+2)
     +)         GO TO 10154
              GO TO 10152
10154         CONTINUE
              IF (.NOT.(IPF.EQ.0)) GO TO 10155
                IAM(IPT+7)=0
                NPF=NPF+1
10155         CONTINUE
10149       CONTINUE
          GO TO 10147
10148     CONTINUE
        IF (.NOT.(NPF.EQ.0)) GO TO 10137
C
C If debugging is turned on, produce a plot.
C
        IF (.NOT.(IDB.NE.0)) GO TO 10156
          CALL ARDBPA (IAM,IDB,'AFTER REMOVING UNCLOSED EDGES')
          IF (ICFELL('ARPRAM',9).NE.0) RETURN
10156   CONTINUE
C
C
10136 CONTINUE
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
      IF (.NOT.(KF3.EQ.0)) GO TO 10157
C
C
        IPT=8
C
10158   CONTINUE
C
C Move to the right across the area map, looking for an edge segment
C that has not yet been completely processed.  If no such segment can
C be found, all subareas have been done.
C
10159     CONTINUE
          IF (.NOT.(MOD(IAM(IPT),4).EQ.3.OR.ABS(IAM(IPT+7)).LT.IAM(6)))
     +    GO TO 10160
            IPT=IAM(IPT+5)
            IF (IPT.EQ.18) GO TO 10161
          GO TO 10159
10160     CONTINUE
C
C Pull out the group identifier for the segment.
C
          IGI=ABS(IAM(IPT+7))
C
C Decide whether to scan the subarea to the left of the edge being
C traced (IPU=1) or the one to the right (IPU=2).
C
          IF (.NOT.(MOD(IAM(IPT),2).EQ.0)) GO TO 10162
            IPU=1
          GO TO 10163
10162     CONTINUE
            IPU=2
10163     CONTINUE
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
10164     CONTINUE
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
10165       CONTINUE
            IF (.NOT.(IAM(IPR+1).EQ.IAM(IAM(IPR+6)+1).AND.IAM(IPR+2).EQ.
     +IAM(IAM(IPR+6)+2))) GO TO 10166
              IPR=IAM(IPR+6)
            GO TO 10165
10166       CONTINUE
C
C Go through the group of nodes, examining all the possible ways to
C move from the current position to a new one.  Pick the direction
C which is leftmost (if IPU=1) or rightmost (if IPU=2).
C
            IP1=IPR
            IP2=IPR
            IPR=0
            IF (.NOT.(IPU.EQ.1)) GO TO 10167
              ANM=-3.14159265358979
            GO TO 10168
10167       CONTINUE
              ANM=+3.14159265358979
10168       CONTINUE
C
10169       CONTINUE
            IF (.NOT.(IAM(IP2+1).EQ.IAM(IP1+1).AND.IAM(IP2+2).EQ.IAM(IP1
     ++2))) GO TO 10170
              IF (.NOT.(ABS(IAM(IAM(IP2+3)+7)).EQ.IGI.AND.(IAM(IAM(IP2+3
     +)+1).NE.IAM(IPP+1).OR.IAM(IAM(IP2+3)+2).NE.IAM(IPP+2)))) GO TO 101
     +71
                IXR=IAM(IAM(IP2+3)+1)
                IYR=IAM(IAM(IP2+3)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10172
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10173
10172           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10174
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10173
10174           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10175
                    GO TO 10028
10175             CONTINUE
                  CALL ARMPIA (IO7,DP2,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10177
                    GO TO 10028
10177             CONTINUE
                  ANG=ARDAT2(DP1,DP2)
10173           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10179
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10180
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=1
10180             CONTINUE
                GO TO 10181
10179           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10182
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=2
10182             CONTINUE
10181           CONTINUE
10171         CONTINUE
              IF (.NOT.(ABS(IAM(IP2+7)).EQ.IGI.AND.(IAM(IAM(IP2+4)+1).NE
     +.IAM(IPP+1).OR.IAM(IAM(IP2+4)+2).NE.IAM(IPP+2)))) GO TO 10183
                IXR=IAM(IAM(IP2+4)+1)
                IYR=IAM(IAM(IP2+4)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10184
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10185
10184           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10186
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10185
10186           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10187
                    GO TO 10028
10187             CONTINUE
                  CALL ARMPIA (IO7,DP2,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10189
                    GO TO 10028
10189             CONTINUE
                  ANG=ARDAT2(DP1,DP2)
10185           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10191
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10192
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=2
10192             CONTINUE
                GO TO 10193
10191           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10194
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=1
10194             CONTINUE
10193           CONTINUE
10183         CONTINUE
              IP2=IAM(IP2+5)
            GO TO 10169
10170       CONTINUE
C
C If no possible exit was found, reverse direction.
C
            IF (.NOT.(IPR.EQ.0)) GO TO 10195
              IPR=IPP
              IPV=3-IPV
              IF (.NOT.(IPU.EQ.1)) GO TO 10196
                ANM=+3.14159265358979
              GO TO 10197
10196         CONTINUE
                ANM=-3.14159265358979
10197         CONTINUE
10195       CONTINUE
C
C Update the total angular change.
C
            ANT=ANT+ANM
C
C Set the marker for the edge segment picked.  If the marker is set
C already, either go back and do a slow-path intersection search or
C log an error.
C
            IF (.NOT.(IPV.EQ.1.AND.MOD(IAM(IPM),2).EQ.0)) GO TO 10198
              IAM(IPM)=IAM(IPM)+1
            GO TO 10199
10198       CONTINUE
            IF (.NOT.(IPV.EQ.2.AND.MOD(IAM(IPM)/2,2).EQ.0)) GO TO 10200
              IAM(IPM)=IAM(IPM)+2
            GO TO 10199
10200       CONTINUE
              IPT=IAM(5)-9
10201         CONTINUE
              IF (.NOT.(IPT.GT.ILW)) GO TO 10202
                IAM(IAM(IPT+4)+3)=IAM(IPT+3)
                IAM(IAM(IPT+3)+4)=IAM(IPT+4)
                IAM(IAM(IPT+6)+5)=IAM(IPT+5)
                IAM(IAM(IPT+5)+6)=IAM(IPT+6)
                IPT=IPT-10
              GO TO 10201
10202         CONTINUE
              IAM(5)=ILW
              DO 10203 IPT=8,IAM(5)-9,10
                IAM(IPT)=4*(IAM(IPT)/4)
10203         CONTINUE
              IF (.NOT.(KF1.NE.0)) GO TO 10204
                KF1=0
                GO TO 101
10204         CONTINUE
                CALL SETER ('ARPRAM - ALGORITHM FAILURE',10,1)
                GO TO 10008
10199       CONTINUE
C
C Exit if we're passing the start of the subarea.
C
            IF (IAM(IPQ+1).EQ.IAM(IPS+1).AND.IAM(IPQ+2).EQ.IAM(IPS+2).AN
     +D.IAM(IPR+1).EQ.IAM(IPT+1).AND.IAM(IPR+2).EQ.IAM(IPT+2)) GO TO 102
     +06
C
          GO TO 10164
10206     CONTINUE
C
C If the closed loop just traced was a hole, insert a temporary
C connecting line to get rid of the hole.
C
          IF (.NOT.((IPU.EQ.1.AND.ANT.LT.0.).OR.(IPU.EQ.2.AND.ANT.GT.0.)
     +))  GO TO 10207
            IOF=0
            XCI=REAL(IAM(IPH+1))
            YCI=REAL(IAM(IPH+2))
            YCO=RLP
            IP1=IPH
10208       CONTINUE
            IF (.NOT.(IAM(IAM(IP1+5)+1).EQ.IAM(IPH+1))) GO TO 10209
              IP1=IAM(IP1+5)
            GO TO 10208
10209       CONTINUE
10210       CONTINUE
            IF (.NOT.(IAM(IP1+1).GE.IAM(IPH+1)-IAM(2))) GO TO 10211
              IF (.NOT.(ABS(IAM(IP1+7)).EQ.IGI.AND.IAM(IAM(IP1+4)+1).GT.
     +IAM(IP1+1).AND.IAM(IAM(IP1+4)+1).GE.IAM(IPH+1))) GO TO 10212
                IF (.NOT.(IAU.EQ.1)) GO TO 10213
                  YTM=REAL(IAM(IP1+2))+
     +            (XCI-REAL(IAM(IP1+1)))*
     +         (REAL(IAM(IAM(IP1+4)+2)-IAM(IP1+2))/
     + REAL(IAM(IAM(IP1+4)+1)-IAM(IP1+1)))
                GO TO 10214
10213           CONTINUE
                  YTM=REAL(DBLE(IAM(IP1+2))+
     +            (DBLE(XCI)-DBLE(IAM(IP1+1)))*
     +         (DBLE(IAM(IAM(IP1+4)+2)-IAM(IP1+2))/
     + DBLE(IAM(IAM(IP1+4)+1)-IAM(IP1+1))))
10214           CONTINUE
                IF (.NOT.(YTM.GT.YCI.AND.YTM.LT.YCO)) GO TO 10215
                  IOF=IP1
                  YCO=YTM
10215           CONTINUE
10212         CONTINUE
              IF (.NOT.(ABS(IAM(IAM(IP1+3)+7)).EQ.IGI.AND.IAM(IAM(IP1+3)
     ++1).GT.IAM(IP1+1).AND.IAM(IAM(IP1+3)+1).GE.IAM(IPH+1))) GO TO 1021
     +6
                IF (.NOT.(IAU.EQ.1)) GO TO 10217
                  YTM=REAL(IAM(IP1+2))+
     +            (XCI-REAL(IAM(IP1+1)))*
     +         (REAL(IAM(IAM(IP1+3)+2)-IAM(IP1+2))/
     + REAL(IAM(IAM(IP1+3)+1)-IAM(IP1+1)))
                GO TO 10218
10217           CONTINUE
                  YTM=REAL(DBLE(IAM(IP1+2))+
     +            (DBLE(XCI)-DBLE(IAM(IP1+1)))*
     +         (DBLE(IAM(IAM(IP1+3)+2)-IAM(IP1+2))/
     + DBLE(IAM(IAM(IP1+3)+1)-IAM(IP1+1))))
10218           CONTINUE
                IF (.NOT.(YTM.GT.YCI.AND.YTM.LT.YCO)) GO TO 10219
                  IOF=IAM(IP1+3)
                  YCO=YTM
10219           CONTINUE
10216         CONTINUE
              IP1=IAM(IP1+6)
            GO TO 10210
10211       CONTINUE
            IF (.NOT.(IOF.NE.0)) GO TO 10220
              IX0=IAM(IPH+1)
              IY0=IAM(IPH+2)
              IF (.NOT.(INT(YCO).NE.IY0)) GO TO 10221
                IPI=18
                L10017=    6
                GO TO 10017
10222           CONTINUE
                IAM(IPN+7)=0
                IAM(IPN+8)=0
                IAM(IPN+9)=0
                IY0=INT(YCO)
                L10017=    7
                GO TO 10017
10223           CONTINUE
                IAM(IPN+7)=LAM-IGI
                IAM(IPN+8)=0
                IAM(IPN+9)=0
10221         CONTINUE
              IF (.NOT.((IX0.NE.IAM(IOF+1).OR.IY0.NE.IAM(IOF+2)).AND.(IX
     +0.NE.IAM(IAM(IOF+4)+1).OR.IY0.NE.IAM(IAM(IOF+4)+2)))) GO TO 10224
                IPI=IOF
                L10017=    8
                GO TO 10017
10225           CONTINUE
10224         CONTINUE
10220       CONTINUE
C
10207     CONTINUE
C
        GO TO 10158
10161   CONTINUE
C
C Zero the lower bits in the markers in all the nodes.
C
        DO 10226 IPT=8,IAM(5)-9,10
          IAM(IPT)=4*(IAM(IPT)/4)
10226   CONTINUE
C
C If debugging is turned on, produce a plot.
C
        IF (.NOT.(IDB.NE.0)) GO TO 10227
          CALL ARDBPA (IAM,IDB,'AFTER LOOKING FOR HOLES')
          IF (ICFELL('ARPRAM',11).NE.0) RETURN
10227   CONTINUE
C
C
10157 CONTINUE
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
10228 CONTINUE
C
C Move to the right across the area map, looking for an edge segment
C that has not yet been completely processed.  If no such segment can
C be found, all subareas have been done.
C
10229   CONTINUE
        IF (.NOT.(MOD(IAM(IPT),4).EQ.3.OR.ABS(IAM(IPT+7)).LT.IAM(6).OR.(
     +KF3.NE.0.AND.((MOD(IAM(IPT),2).NE.0.OR.IAM(IPT+8).GT.0).AND.(MOD(I
     +AM(IPT)/2,2).NE.0.OR.IAM(IPT+9).GT.0))))) GO TO 10230
          IPT=IAM(IPT+5)
          IF (IPT.EQ.18) GO TO 10231
        GO TO 10229
10230   CONTINUE
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
     +))GO TO 10232
          IPU=1
        GO TO 10233
10232   CONTINUE
          IPU=2
10233   CONTINUE
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
10234   CONTINUE
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
10235     CONTINUE
          IF (.NOT.(IAM(IPR+1).EQ.IAM(IAM(IPR+6)+1).AND.IAM(IPR+2).EQ.IA
     +M(IAM(IPR+6)+2))) GO TO 10236
            IPR=IAM(IPR+6)
          GO TO 10235
10236     CONTINUE
C
C If there is only one node in the group, the exit path is obvious.
C
          IF (.NOT.(IAM(IPR+1).NE.IAM(IAM(IPR+5)+1).OR.IAM(IPR+2).NE.IAM
     +(IAM(IPR+5)+2))) GO TO 10237
            IF (.NOT.(IAM(IAM(IPR+3)+1).NE.IAM(IPP+1).OR.IAM(IAM(IPR+3)+
     +2).NE.IAM(IPP+2))) GO TO 10238
              IF (.NOT.(IAM(IAM(IPR+3)+7).EQ.LAM-IGI.OR.ABS(IAM(IAM(IPR+
     +3)+7)).EQ.IGI)) GO TO 10239
                IPM=IAM(IPR+3)
                IPR=IPM
                IPV=IPU
              GO TO 10240
10239         CONTINUE
                IPR=0
10240         CONTINUE
            GO TO 10241
10238       CONTINUE
              IF (.NOT.(IAM(IPR+7).EQ.LAM-IGI.OR.ABS(IAM(IPR+7)).EQ.IGI)
     +)       GO TO 10242
                IPM=IPR
                IPR=IAM(IPR+4)
                IPV=3-IPU
              GO TO 10243
10242         CONTINUE
                IPR=0
10243         CONTINUE
10241       CONTINUE
C
C Otherwise, go through the group of nodes, examining all the possible
C ways to move from the current position to a new one.  Pick the
C direction which is leftmost (if IPU=1) or rightmost (if IPU=2).
C
          GO TO 10244
10237     CONTINUE
C
            IP1=IPR
            IP2=IPR
            IPR=0
            IF (.NOT.(IPU.EQ.1)) GO TO 10245
              ANM=-3.14159265358979
            GO TO 10246
10245       CONTINUE
              ANM=+3.14159265358979
10246       CONTINUE
C
10247       CONTINUE
            IF (.NOT.(IAM(IP2+1).EQ.IAM(IP1+1).AND.IAM(IP2+2).EQ.IAM(IP1
     ++2))) GO TO 10248
              IF (.NOT.((IAM(IAM(IP2+3)+7).EQ.LAM-IGI.OR.ABS(IAM(IAM(IP2
     ++3)+7)).EQ.IGI).AND.(IAM(IAM(IP2+3)+1).NE.IAM(IPP+1).OR.IAM(IAM(IP
     +2+3)+2).NE.IAM(IPP+2)))) GO TO 10249
                IXR=IAM(IAM(IP2+3)+1)
                IYR=IAM(IAM(IP2+3)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10250
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10251
10250           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10252
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10251
10252           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10253
                    GO TO 10028
10253             CONTINUE
                  CALL ARMPIA (IO7,DP2,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10255
                    GO TO 10028
10255             CONTINUE
                  ANG=ARDAT2(DP1,DP2)
10251           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10257
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10258
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=1
10258             CONTINUE
                GO TO 10259
10257           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10260
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=2
10260             CONTINUE
10259           CONTINUE
10249         CONTINUE
              IF (.NOT.((IAM(IP2+7).EQ.LAM-IGI.OR.ABS(IAM(IP2+7)).EQ.IGI
     +).AND.(IAM(IAM(IP2+4)+1).NE.IAM(IPP+1).OR.IAM(IAM(IP2+4)+2).NE.IAM
     +(IPP+2)))) GO TO 10261
                IXR=IAM(IAM(IP2+4)+1)
                IYR=IAM(IAM(IP2+4)+2)
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10262
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10263
10262           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10264
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10263
10264           CONTINUE
                  IO6(3,1)=IXQ-IXP
                  IO6(3,2)=IYR-IYQ
                  IO6(3,3)=IYQ-IYP
                  IO6(3,4)=IXR-IXQ
                  CALL ARMPIA (IO6,DP1,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10265
                    GO TO 10028
10265             CONTINUE
                  CALL ARMPIA (IO7,DP2,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10267
                    GO TO 10028
10267             CONTINUE
                  ANG=ARDAT2(DP1,DP2)
10263           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10269
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10270
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=2
10270             CONTINUE
                GO TO 10271
10269           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10272
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=1
10272             CONTINUE
10271           CONTINUE
10261         CONTINUE
              IP2=IAM(IP2+5)
            GO TO 10247
10248       CONTINUE
C
10244     CONTINUE
C
C If no possible exit was found, reverse direction.
C
          IF (.NOT.(IPR.EQ.0)) GO TO 10273
            IPR=IPP
            IPV=3-IPV
10273     CONTINUE
C
C Update the markers for the edge segment picked.
C
          IF (.NOT.(IPV.EQ.1.AND.MOD(IAM(IPM),2).EQ.0)) GO TO 10274
            IAM(IPM)=IAM(IPM)+1
            IAQ=IPM+8
          GO TO 10275
10274     CONTINUE
          IF (.NOT.(IPV.EQ.2.AND.MOD(IAM(IPM)/2,2).EQ.0)) GO TO 10276
            IAM(IPM)=IAM(IPM)+2
            IAQ=IPM+9
          GO TO 10275
10276     CONTINUE
            CALL SETER ('ARPRAM - ALGORITHM FAILURE',12,1)
            GO TO 10008
10275     CONTINUE
C
          IF (.NOT.(IAM(IAQ).LE.0.OR.IAM(IAQ).GE.IAM(6))) GO TO 10278
            IF (.NOT.(IAM(IAQ).LT.0)) GO TO 10279
              IAI=-1
              IAX=LAM*4
            GO TO 10280
10279       CONTINUE
            IF (.NOT.(IAM(IAQ).GE.IAM(6).AND.IAM(IPM).GT.IAX)) GO TO 102
     +81
              IAI=IAM(IAQ)
              IAX=IAM(IPM)
10280       CONTINUE
10281       CONTINUE
            IAM(IAQ)=IAP
            IAP=IAQ
10278     CONTINUE
C
C Exit if we're passing the start of the subarea.
C
          IF (IAM(IPQ+1).EQ.IAM(IPS+1).AND.IAM(IPQ+2).EQ.IAM(IPS+2).AND.
     +IAM(IPR+1).EQ.IAM(IPT+1).AND.IAM(IPR+2).EQ.IAM(IPT+2)) GO TO 10282
C
        GO TO 10234
10282   CONTINUE
C
C Go through the chain of area identifiers, updating them.
C
10283   CONTINUE
        IF (.NOT.(IAP.NE.0)) GO TO 10284
          IAQ=IAM(IAP)
          IAM(IAP)=IAI
          IAP=IAQ
        GO TO 10283
10284   CONTINUE
C
C If a zero identifier was selected for the area, see if the search for
C holes was suppressed and, if so, re-do it.
C
        IF (.NOT.(IAI.EQ.0.AND.KF3.NE.0)) GO TO 10285
          DO 10286 IPT=8,IAM(5)-9,10
            IAM(IPT)=4*(IAM(IPT)/4)
10286     CONTINUE
          KF3=0
          GO TO 104
10285   CONTINUE
C
      GO TO 10228
10231 CONTINUE
C
C Delete the nodes used to put in the temporary connecting lines.
C
      IPT=IAM(5)-9
C
10287 CONTINUE
      IF (.NOT.(IPT.GT.ILW)) GO TO 10288
        IAM(IAM(IPT+4)+3)=IAM(IPT+3)
        IAM(IAM(IPT+3)+4)=IAM(IPT+4)
        IAM(IAM(IPT+6)+5)=IAM(IPT+5)
        IAM(IAM(IPT+5)+6)=IAM(IPT+6)
        IPT=IPT-10
      GO TO 10287
10288 CONTINUE
C
      IAM(5)=ILW
C
C Zero the markers in all the remaining nodes.
C
      DO 10289 IPT=8,IAM(5)-9,10
        IAM(IPT)=0
10289 CONTINUE
C
C Update the map state.
C
      IAM(4)=1
C
C If debugging is turned on, produce a plot.
C
      IF (.NOT.(IDB.NE.0)) GO TO 10290
        CALL ARDBPA (IAM,IDB,'AFTER UPDATING AREA IDENTIFIERS')
        IF (ICFELL('ARPRAM',13).NE.0) RETURN
10290 CONTINUE
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
10017 CONTINUE
        IPN=IAM(5)+1
        IF (.NOT.(IAM(5)+10.GE.IAM(6))) GO TO 10291
          CALL SETER ('ARPRAM - AREA-MAP ARRAY OVERFLOW',14,1)
          GO TO 10008
10291   CONTINUE
        IAM(5)=IAM(5)+10
        IAM(IPN)=IAM(IPI)
        IAM(IPN+1)=IX0
        IAM(IPN+2)=IY0
        IAM(IPN+3)=IPI
        IAM(IPN+4)=IAM(IPI+4)
        IAM(IAM(IPI+4)+3)=IPN
        IAM(IPI+4)=IPN
10293   CONTINUE
          IF (.NOT.(IAM(IPN+1).LT.IAM(IPX+1))) GO TO 10294
            IPX=IAM(IPX+6)
          GO TO 10295
10294     CONTINUE
          IF (.NOT.(IAM(IPN+1).GT.IAM(IAM(IPX+5)+1))) GO TO 10296
            IPX=IAM(IPX+5)
          GO TO 10295
10296     CONTINUE
10297       CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IPX+1).AND.IAM(IPN+2).LT.IAM(I
     +PX+2))) GO TO 10298
                IPX=IAM(IPX+6)
              GO TO 10299
10298         CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IAM(IPX+5)+1).AND.IAM(IPN+2).G
     +T.IAM(IAM(IPX+5)+2))) GO TO 10300
                IPX=IAM(IPX+5)
              GO TO 10299
10300         CONTINUE
10301           CONTINUE
                IF (.NOT.(IAM(IAM(IPX+5)+1).EQ.IAM(IPN+1).AND.IAM(IAM(IP
     +X+5)+2).EQ.IAM(IPN+2))) GO TO 10302
                  IPX=IAM(IPX+5)
                GO TO 10301
10302           CONTINUE
                GO TO 10303
10299         CONTINUE
            GO TO 10297
10303       CONTINUE
            GO TO 10304
10295     CONTINUE
        GO TO 10293
10304   CONTINUE
        IAM(IPN+5)=IAM(IPX+5)
        IAM(IPN+6)=IAM(IAM(IPX+5)+6)
        IAM(IAM(IPX+5)+6)=IPN
        IAM(IPX+5)=IPN
        IAM(IPN+7)=IAM(IPI+7)
        IAM(IPN+8)=IAM(IPI+8)
        IAM(IPN+9)=IAM(IPI+9)
      GO TO (10016,10032,10083,10085,10097,10222,10223,10225) , L10017
C
C This internal procedure is called when an error occurs in ARMPIA.
C
10028 CONTINUE
        CALL SETER
     +  ('ARPRAM/ARMPIA - MULTIPLE-PRECISION QUANTITY IS TOO BIG',
     +                                                       15,1)
        GO TO 10008
C
C This internal procedure cleans up after an error condition occurs.
C It removes nodes used to put in temporary connecting lines, if any,
C and returns markers in the remaining nodes to zero.
C
10008 CONTINUE
C
C Delete new nodes from the area map.
C
        IPT=IAM(5)-9
C
10306   CONTINUE
        IF (.NOT.(IPT.GT.ILW)) GO TO 10307
          IAM(IAM(IPT+4)+3)=IAM(IPT+3)
          IAM(IAM(IPT+3)+4)=IAM(IPT+4)
          IAM(IAM(IPT+6)+5)=IAM(IPT+5)
          IAM(IAM(IPT+5)+6)=IAM(IPT+6)
          IPT=IPT-10
        GO TO 10306
10307   CONTINUE
C
        IAM(5)=ILW
C
C Zero the low-order bits of the markers in all the remaining nodes.
C
        DO 10308 IPT=8,IAM(5)-9,10
          IAM(IPT)=4*(IAM(IPT)/4)
10308   CONTINUE
C
C If appropriate, delete space temporarily used at the upper end of
C the area map array.
C
        IF (ISU.NE.0) IAM(6)=ISU
C
C Return to the calling routine.
C
        RETURN
C
C
      END
