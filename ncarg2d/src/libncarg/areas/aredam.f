C
C $Id: aredam.f,v 1.3 1993-09-23 17:24:55 kennison Exp $
C
      SUBROUTINE AREDAM (IAM,XCA,YCA,LCA,IGI,IDL,IDR)
C
      DIMENSION IAM(*),XCA(*),YCA(*)
C
C The routine AREDAM allows the caller to add an edge, separating two
C areas from each other, to an existing area map.  The input arguments
C are as follows:
C
C IAM is an integer array (dimensioned as specified by a prior call to
C the routine ARINAM) in which resides the area map to which an edge is
C to be added.  The user should make no direct changes in the area map;
C only routines in the package AREAS should be allowed to modify it.
C
C XCA is an array of x coordinates of edge points.
C
C YCA is an array of y coordinates of edge points.
C
C LCA is the number of edge points defined by XCA and YCA.
C
C IGI is the identifier of the group to which this edge belongs.
C
C IDL is the identifier of the area to the left of the new edge, in the
C current user coordinate system.
C
C IDR is the identifier of the area to the right of the new edge, in the
C current user coordinate system.
C
C Upon return from AREDAM, all arguments are unchanged except IAM, which
C contains the augmented area map.
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
C Pull out the length of the area map and check for initialization.
C
      LAM=IAM(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAM(LAM).NE.LAM)) GO TO 10001
        CALL SETER ('AREDAM - INITIALIZATION DONE IMPROPERLY',1,1)
        RETURN
10001 CONTINUE
C
C Pull out the current value of the pointer IPX.
C
      IPX=IAM(3)
C
C Use GETSET to set up parameters allowing us to map x and y coordinates
C from the user system to the local integer system.
C
      CALL GETSET (FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL)
      ILX=(ILL-1)/2
      ILY=MOD(ILL-1,2)
C
C Set the left and right area identifiers for the edge in the viewport.
C
      IF (.NOT.((FUL.LT.FUR.AND.FUB.LT.FUT).OR.(FUL.GT.FUR.AND.FUB.GT.FU
     +T))) GO TO 10002
        JDL=IDL
        JDR=IDR
      GO TO 10003
10002 CONTINUE
        JDL=IDR
        JDR=IDL
10003 CONTINUE
C
C Add the group identifier to the list at the end of the area map (if
C it's not already there) and set IGN to its index.  If the identifier
C is a new one, provide a boundary rectangle.
C
      IGN=LAM
C
10004 CONTINUE
        IGN=IGN-1
        IF (.NOT.(IGN.GE.IAM(6))) GO TO 10005
          IF (MOD(IAM(IGN),2).EQ.0.AND.IAM(IGN)/2.EQ.IGI) GO TO 10006
        GO TO 10007
10005   CONTINUE
          IF (.NOT.(IGN.LE.IAM(5)+50)) GO TO 10008
            CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',2,1)
            RETURN
10008     CONTINUE
          IAM(6)=IGN
          IAM(IGN)=IGI*2
          ILN=-1
          IRN=0
          IXN=0
          IYN=0
          L10010=    1
          GO TO 10010
10009     CONTINUE
          IAM(IPN+7)=0
          IAM(IPN+8)=0
          IYN=ILC
          L10010=    2
          GO TO 10010
10011     CONTINUE
          IXN=ILC
          L10010=    3
          GO TO 10010
10012     CONTINUE
          IYN=0
          L10010=    4
          GO TO 10010
10013     CONTINUE
          IXN=0
          L10010=    5
          GO TO 10010
10014     CONTINUE
          IAM(7)=IAM(7)+1
          GO TO 10006
10007   CONTINUE
      GO TO 10004
10006 CONTINUE
C
C Add the area identifiers to the list at the end of the area map (if
C they're not already there) and set ILN and IRN to their indices.
C
      IF (.NOT.(JDL.LE.0)) GO TO 10015
        ILN=JDL
      GO TO 10016
10015 CONTINUE
        ILN=LAM
10017   CONTINUE
          ILN=ILN-1
          IF (.NOT.(ILN.GE.IAM(6))) GO TO 10018
            IF (MOD(IAM(ILN),2).EQ.1.AND.IAM(ILN)/2.EQ.JDL) GO TO 10019
          GO TO 10020
10018     CONTINUE
            IF (.NOT.(ILN.LE.IAM(5))) GO TO 10021
              CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',3,1)
              RETURN
10021       CONTINUE
            IAM(6)=ILN
            IAM(ILN)=JDL*2+1
            GO TO 10019
10020     CONTINUE
        GO TO 10017
10019   CONTINUE
10016 CONTINUE
C
      IF (.NOT.(JDR.LE.0)) GO TO 10022
        IRN=JDR
      GO TO 10023
10022 CONTINUE
        IRN=LAM
10024   CONTINUE
          IRN=IRN-1
          IF (.NOT.(IRN.GE.IAM(6))) GO TO 10025
            IF (MOD(IAM(IRN),2).EQ.1.AND.IAM(IRN)/2.EQ.JDR) GO TO 10026
          GO TO 10027
10025     CONTINUE
            IF (.NOT.(IRN.LE.IAM(5))) GO TO 10028
              CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',4,1)
              RETURN
10028       CONTINUE
            IAM(6)=IRN
            IAM(IRN)=JDR*2+1
            GO TO 10026
10027     CONTINUE
        GO TO 10024
10026   CONTINUE
10023 CONTINUE
C
C Make sure there's room for LCA points in the area map.
C
      IF (.NOT.(IAM(5)+LCA*10.GE.IAM(6))) GO TO 10029
        CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',5,1)
        RETURN
10029 CONTINUE
C
C Proceed with adding the user's edge to the area map.  IXL and IYL are
C the coordinates of the last point inserted in the area map.  IGL is
C the group identifier of the last point.
C
      IXL=IAM(IAM(22)+1)
      IYL=IAM(IAM(22)+2)
      IGL=IAM(IAM(22)+7)
C
C Loop through the points in the user list.
C
        ICA = 1
        GO TO 10032
10030   CONTINUE
        ICA =ICA +1
10032   CONTINUE
        IF (ICA .GT.(LCA)) GO TO 10031
C
C Get the x and y coordinates of the next point.
C
        IF (.NOT.(ILX.EQ.0)) GO TO 10033
          IXN=MAX(0,MIN(ILC,
     +          INT(RLC*(FFL+(FFR-FFL)*(XCA(ICA)-FUL)/(FUR-FUL))+.5)))
        GO TO 10034
10033   CONTINUE
          IXN=MAX(0,MIN(ILC,
     +        INT(RLC*(FFL+(FFR-FFL)*(ALOG(XCA(ICA))-ALOG(FUL))/
     +                                    (ALOG(FUR)-ALOG(FUL)))+.5)))
10034   CONTINUE
        IF (.NOT.(ILY.EQ.0)) GO TO 10035
          IYN=MAX(0,MIN(ILC,
     +          INT(RLC*(FFB+(FFT-FFB)*(YCA(ICA)-FUB)/(FUT-FUB))+.5)))
        GO TO 10036
10035   CONTINUE
          IYN=MAX(0,MIN(ILC,
     +        INT(RLC*(FFB+(FFT-FFB)*(ALOG(YCA(ICA))-ALOG(FUB))/
     +                                    (ALOG(FUT)-ALOG(FUB)))+.5)))
10036   CONTINUE
C
C If the point to be inserted is distinct from the old point, add it.
C If it's the first point, zero its group and area identifiers.  Update
C the descriptors of the last point.
C
        IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL.OR.IGN.NE.IGL)) GO TO 10037
          L10010=    6
          GO TO 10010
10038     CONTINUE
          IF (.NOT.(ICA.EQ.1)) GO TO 10039
            IAM(IPN+7)=0
            IAM(IPN+8)=0
            IAM(IPN+9)=0
10039     CONTINUE
          IXL=IXN
          IYL=IYN
          IGL=IGN
10037   CONTINUE
C
      GO TO 10030
10031 CONTINUE
C
C Restore the value of the pointer IPX to its position in the area map.
C
      IAM(3)=IPX
C
C Set the map state to say that an edge has been entered.
C
      IAM(4)=0
C
C Done.
C
      RETURN
C
C This internal procedure adds a new point to the area map.
C
10010 CONTINUE
        IPN=IAM(5)+1
        IAM(5)=IAM(5)+10
        IAM(IPN)=IAM(IAM(22))+4
        IAM(IPN+1)=IXN
        IAM(IPN+2)=IYN
        IAM(IPN+3)=18
        IAM(IPN+4)=IAM(22)
        IAM(IAM(22)+3)=IPN
        IAM(22)=IPN
10040   CONTINUE
          IF (.NOT.(IAM(IPN+1).LT.IAM(IPX+1))) GO TO 10041
            IPX=IAM(IPX+6)
          GO TO 10042
10041     CONTINUE
          IF (.NOT.(IAM(IPN+1).GT.IAM(IAM(IPX+5)+1))) GO TO 10043
            IPX=IAM(IPX+5)
          GO TO 10042
10043     CONTINUE
10044       CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IPX+1).AND.IAM(IPN+2).LT.IAM(I
     +PX+2))) GO TO 10045
                IPX=IAM(IPX+6)
              GO TO 10046
10045         CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IAM(IPX+5)+1).AND.IAM(IPN+2).G
     +T.IAM(IAM(IPX+5)+2))) GO TO 10047
                IPX=IAM(IPX+5)
              GO TO 10046
10047         CONTINUE
                GO TO 10048
10046         CONTINUE
            GO TO 10044
10048       CONTINUE
            GO TO 10049
10042     CONTINUE
        GO TO 10040
10049   CONTINUE
        IAM(IPN+5)=IAM(IPX+5)
        IAM(IPN+6)=IAM(IAM(IPX+5)+6)
        IAM(IAM(IPX+5)+6)=IPN
        IAM(IPX+5)=IPN
        IAM(IPN+7)=IGN
        IAM(IPN+8)=ILN
        IAM(IPN+9)=IRN
      GO TO (10009,10011,10012,10013,10014,10038) , L10010
C
      END
