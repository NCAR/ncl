C
C $Id: aredam.f,v 1.10 1998-05-13 17:15:39 kennison Exp $
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
C XCA is an array of X coordinates of edge points.
C
C YCA is an array of Y coordinates of edge points.
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
     +                IDC,IDI,IRC(16),RLA,RWA,RDI,RSI
      SAVE   /ARCOMN/
C
C Declare the BLOCK DATA routine external, which should force it to
C load from a binary library.
C
      EXTERNAL ARBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('AREDAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pull out the length of the area map and check for initialization.
C
      LAM=IAM(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAM(LAM).NE.LAM)) GO TO 10001
        CALL SETER ('AREDAM - INITIALIZATION DONE IMPROPERLY',2,1)
        RETURN
10001 CONTINUE
C
C Pull out the current value of the pointer IPX.
C
      IPX=IAM(3)
C
C Use GETSET to set up parameters allowing us to map X and Y coordinates
C from the user system to the local integer system.
C
      CALL GETSET (FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL)
      IF (ICFELL('AREDAM',3).NE.0) RETURN
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
            CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',4,1)
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
              CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',5,1)
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
              CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',6,1)
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
C Make sure there's room for 3*LCA/2 points in the area map.  (Even
C with increases due to clipping, this is the largest number of points
C that could be added to the area map by an LCA-point edge.)
C
      IF (.NOT.(IAM(5)+(3*LCA/2)*10.GE.IAM(6))) GO TO 10029
        CALL SETER ('AREDAM - AREA-MAP ARRAY OVERFLOW',7,1)
        RETURN
10029 CONTINUE
C
C Add the points of the user's edge to the area map.  IXL and IYL are
C the coordinates of the last point inserted in the area map, and IGL
C is the group identifier of the last point.
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
C Get the X and Y coordinates of the next point.
C
        IF (.NOT.(ILX.EQ.0)) GO TO 10033
          XNX=RLC*(FFL+(FFR-FFL)*(XCA(ICA)-FUL)/(FUR-FUL))
        GO TO 10034
10033   CONTINUE
          XNX=RLC*(FFL+(FFR-FFL)*(ALOG(XCA(ICA))-ALOG(FUL))/
     +                           (ALOG(     FUR)-ALOG(FUL)))
10034   CONTINUE
C
        IF (.NOT.(ILY.EQ.0)) GO TO 10035
          YNX=RLC*(FFB+(FFT-FFB)*(YCA(ICA)-FUB)/(FUT-FUB))
        GO TO 10036
10035   CONTINUE
          YNX=RLC*(FFB+(FFT-FFB)*(ALOG(YCA(ICA))-ALOG(FUB))/
     +                           (ALOG(     FUT)-ALOG(FUB)))
10036   CONTINUE
C
C Clip the edge against the viewport and put the resulting pieces into
C the area map.
C
C Compute a "next-point-outside-window" flag.  The value of this flag
C is between -4 and +4, depending on where the next point is relative
C to the window, as shown in the following diagram:
C
C                      |      |
C                   -2 |  +1  | +4
C            YMAX -----+------+-----
C                   -3 |   0  | +3
C            YMIN -----+------+-----
C                   -4 |  -1  | +2
C                      |      |
C                    XMIN    XMAX
C
C Ultimately, we combine the values of this flag for two consecutive
C points in such a way as to get an integer between 1 and 81, telling
C us what combination of inside/outside we have to deal with.
C
        NPO=IFIX(3.*(SIGN(.51,XNX)+SIGN(.51,XNX-RLC))+
     +              (SIGN(.51,YNX)+SIGN(.51,YNX-RLC)))
C
        IF (.NOT.(ICA.EQ.1)) GO TO 10037
          IF (.NOT.(NPO.EQ.0)) GO TO 10038
            IXN=NINT(XNX)
            IYN=NINT(YNX)
            IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL.OR.IGN.NE.IGL)) GO TO 100
     +39
              L10010=    6
              GO TO 10010
10040         CONTINUE
              IAM(IPN+7)=0
              IAM(IPN+8)=0
              IAM(IPN+9)=0
              IGL=IGN
10039       CONTINUE
10038     CONTINUE
        GO TO 10041
10037   CONTINUE
          IF (.NOT.(LPO.EQ.0)) GO TO 10042
            IF (.NOT.(NPO.EQ.0)) GO TO 10043
              IXN=NINT(XNX)
              IYN=NINT(YNX)
              IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL)) GO TO 10044
                L10010=    7
                GO TO 10010
10045           CONTINUE
10044         CONTINUE
              GO TO 112
10043       CONTINUE
              XPE=XLS
              YPE=YLS
              XDI=XNX-XLS
              YDI=YNX-YLS
              IF (.NOT.(ABS(XDI).GT..000001*RLC)) GO TO 10046
                XPE=0.
                IF (XDI.GE.0.) XPE=RLC
                YPE=YLS+(XPE-XLS)*YDI/XDI
                IF (YPE.GE.0..AND.YPE.LE.RLC) GO TO 101
10046         CONTINUE
              IF (.NOT.(ABS(YDI).GT..000001*RLC)) GO TO 10047
                YPE=0.
                IF (YDI.GE.0.) YPE=RLC
                XPE=XLS+(YPE-YLS)*XDI/YDI
10047         CONTINUE
  101         IXN=NINT(XPE)
              IYN=NINT(YPE)
              IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL)) GO TO 10048
                L10010=    8
                GO TO 10010
10049           CONTINUE
10048         CONTINUE
              GO TO 112
10042     CONTINUE
            IF (.NOT.(NPO.EQ.0)) GO TO 10050
              XPE=XNX
              YPE=YNX
              XDI=XLS-XNX
              YDI=YLS-YNX
              IF (.NOT.(ABS(XDI).GT..000001*RLC)) GO TO 10051
                XPE=0.
                IF (XDI.GE.0.) XPE=RLC
                YPE=YNX+(XPE-XNX)*YDI/XDI
                IF (YPE.GE.0..AND.YPE.LE.RLC) GO TO 102
10051         CONTINUE
              IF (.NOT.(ABS(YDI).GT..000001*RLC)) GO TO 10052
                YPE=0.
                IF (YDI.GE.0.) YPE=RLC
                XPE=XNX+(YPE-YNX)*XDI/YDI
10052         CONTINUE
  102         IXN=NINT(XPE)
              IYN=NINT(YPE)
              IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL)) GO TO 10053
                L10010=    9
                GO TO 10010
10054           CONTINUE
                IAM(IPN+7)=0
                IAM(IPN+8)=0
                IAM(IPN+9)=0
10053         CONTINUE
              IXN=NINT(XNX)
              IYN=NINT(YNX)
              IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL)) GO TO 10055
                L10010=   10
                GO TO 10010
10056           CONTINUE
10055         CONTINUE
              GO TO 112
10050       CONTINUE
              MPOW=9*LPO+NPO+41
              GO TO ( 112,112,112,112,112,103,112,103,103,
     +                112,112,112,104,112,103,104,103,103,
     +                112,112,112,104,112,112,104,104,112,
     +                112,106,106,112,112,103,112,103,103,
     +                112,112,112,112,112,112,112,112,112,
     +                105,105,112,105,112,112,104,104,112,
     +                112,106,106,112,112,106,112,112,112,
     +                105,105,106,105,112,106,112,112,112,
     +                105,105,112,105,112,112,112,112,112 ) , MPOW
C
  103         XE1=0.
              YT1=0.
              XE2=RLC
              YT2=RLC
              GO TO 107
  104         XE1=0.
              YT1=RLC
              XE2=RLC
              YT2=0.
              GO TO 107
  105         XE1=RLC
              YT1=RLC
              XE2=0.
              YT2=0.
              GO TO 107
  106         XE1=RLC
              YT1=0.
              XE2=0.
              YT2=RLC
  107         XDI=XNX-XLS
              YDI=YNX-YLS
              IF (ABS(XDI).LE..000001*RLC) GO TO 109
              YE1=YLS+(XE1-XLS)*YDI/XDI
              YE2=YLS+(XE2-XLS)*YDI/XDI
              IF (.NOT.(ABS(YDI).LE..000001*RLC)) GO TO 10057
                IF (YE1.LT.0..OR.YE1.GT.RLC) GO TO 112
                IF (YE2.LT.0..OR.YE2.GT.RLC) GO TO 112
                GO TO 111
10057         CONTINUE
              IF (YE1.GE.0..AND.YE1.LE.RLC) GO TO 108
              YE1=YT1
              XE1=XLS+(YE1-YLS)*XDI/YDI
              IF (XE1.LT.0..OR.XE1.GT.RLC) GO TO 112
  108         IF (YE2.GE.0..AND.YE2.LE.RLC) GO TO 111
              GO TO 110
  109         YE1=YT1
              XE1=XLS+(YE1-YLS)*XDI/YDI
              IF (XE1.LT.0..OR.XE1.GT.RLC) GO TO 112
  110         YE2=YT2
              XE2=XLS+(YE2-YLS)*XDI/YDI
              IF (XE2.LT.0..OR.XE2.GT.RLC) GO TO 112
  111         IXN=NINT(XE1)
              IYN=NINT(YE1)
              IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL)) GO TO 10058
                L10010=   11
                GO TO 10010
10059           CONTINUE
                IAM(IPN+7)=0
                IAM(IPN+8)=0
                IAM(IPN+9)=0
10058         CONTINUE
              IXN=NINT(XE2)
              IYN=NINT(YE2)
              IF (.NOT.(IXN.NE.IXL.OR.IYN.NE.IYL)) GO TO 10060
                L10010=   12
                GO TO 10010
10061           CONTINUE
10060         CONTINUE
              GO TO 112
10041   CONTINUE
C
C Processing of the next point is done.  It becomes the last point and
C we return to the user for a new next point.
C
  112   XLS=XNX
        YLS=YNX
        LPO=NPO
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
C This internal procedure adds the point (IXN,IYN) to the area map,
C using identifiers IGN, ILN, and IRN.  It also sets IXL = IXN and
C IYL = IYN.
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
10062   CONTINUE
          IF (.NOT.(IAM(IPN+1).LT.IAM(IPX+1))) GO TO 10063
            IPX=IAM(IPX+6)
          GO TO 10064
10063     CONTINUE
          IF (.NOT.(IAM(IPN+1).GT.IAM(IAM(IPX+5)+1))) GO TO 10065
            IPX=IAM(IPX+5)
          GO TO 10064
10065     CONTINUE
10066       CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IPX+1).AND.IAM(IPN+2).LT.IAM(I
     +PX+2))) GO TO 10067
                IPX=IAM(IPX+6)
              GO TO 10068
10067         CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IAM(IPX+5)+1).AND.IAM(IPN+2).G
     +T.IAM(IAM(IPX+5)+2))) GO TO 10069
                IPX=IAM(IPX+5)
              GO TO 10068
10069         CONTINUE
                GO TO 10070
10068         CONTINUE
            GO TO 10066
10070       CONTINUE
            GO TO 10071
10064     CONTINUE
        GO TO 10062
10071   CONTINUE
        IAM(IPN+5)=IAM(IPX+5)
        IAM(IPN+6)=IAM(IAM(IPX+5)+6)
        IAM(IAM(IPX+5)+6)=IPN
        IAM(IPX+5)=IPN
        IAM(IPN+7)=IGN
        IAM(IPN+8)=ILN
        IAM(IPN+9)=IRN
        IXL=IXN
        IYL=IYN
      GO TO (10009,10011,10012,10013,10014,10040,10045,10049,10054,10056
     +,10059,10061) , L10010
C
      END
