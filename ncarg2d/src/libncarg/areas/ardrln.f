C
C	$Id: ardrln.f,v 1.1.1.1 1992-04-17 22:32:11 ncargd Exp $
C
C
C The subroutine ARDRLN.
C --- ---------- -------
C
      SUBROUTINE ARDRLN (IAM,XCD,YCD,NCD,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      DIMENSION IAM(*),XCD(*),YCD(*),XCS(*),YCS(*),IAI(*),IAG(*)
C
C The routine ARDRLN allows the caller to draw a polyline across the
C area represented by an area map.  The polyline is broken into pieces
C by the boundary lines in the map.  For each piece, the user routine
C LPR is called.
C
C IAM is the array holding the area map, created by prior calls to the
C routines ARINAM and AREDAM.
C
C The arrays XCD and YCD hold the NCD coordinates of the points defining
C the polyline.  Coordinates are given in the current user system, as
C defined by the last SET call.
C
C The arrays XCS and YCS are used, in a call to LPR, to hold the X
C and Y coordinates of points defining a particular subline.  Each is
C dimensioned MCS.
C
C The arrays IAG and IAI are used, in a call to LPR, to hold group and
C area identifiers of the subline defined by XCS and YCS.  Each is
C dimensioned MAI.
C
C LPR is the user's line-processing routine.  It must be declared in
C an EXTERNAL statement in the routine which calls ARDRLN.  It will be
C called using a statement like
C
C       CALL LPR (XCS,YCS,NCS,IAI,IAG,NAI)
C
C where XCS and YCS hold the normalized device coordinates of NCS points
C defining a portion of the original polyline and IAI and IAG hold NAI
C area-identifier/group-identifier pairs for the area within which that
C piece of the polyline lies.
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
C Define a few double precision variables.
C
      DOUBLE PRECISION DPT,DX0,DY0
C
C Declare the arrays which keep track of intersection points.
C
      DIMENSION XCI(10),YCI(10),DSI(10)
C
C Define the arrays which determine the multiple-precision operations
C to be done by ARMPIA.
C
      DIMENSION IO1(4,8),IO2(4,18),IO3(4,4)
C
      DATA IO1 / 1 ,  1 ,  0 ,  0 ,
     +           1 ,  2 ,  0 ,  0 ,
     +           1 ,  3 ,  0 ,  0 ,
     +           1 ,  4 ,  0 ,  0 ,
     +           4 ,  5 ,  1 ,  2 ,
     +           4 ,  6 ,  3 ,  4 ,
     +           3 ,  7 ,  5 ,  6 ,
     +           5 ,  7 ,  0 ,  0 /
      DATA IO2 / 1 ,  7 ,  0 ,  0 ,
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
      DATA IO3 / 4 ,  7 ,  2 ,  5 ,
     +           4 ,  8 ,  4 ,  6 ,
     +           3 ,  9 ,  7 ,  8 ,
     +           5 ,  9 ,  0 ,  0 /
C
C Pull out the length of the area map and check for initialization.
C
      LAM=IAM(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAM(LAM).NE.LAM)) GO TO 10001
        CALL SETER ('ARDRLN - INITIALIZATION DONE IMPROPERLY',1,1)
        RETURN
10001 CONTINUE
C
C If there are too few points in the input arrays, do nothing.
C
      IF (NCD.LE.1) RETURN
C
C If it has not already been done, find points of intersection and
C incorporate them into the map and then adjust area identifiers.
C
      IF (IAM(4).EQ.0) CALL ARPRAM (IAM,0,0,0)
C
C Pull out the current value of the pointer IPX.
C
      IPX=IAM(3)
C
C Use GETSET to set up parameters allowing us to map X and Y coordinates
C from the user system to the local integer system.
C
      CALL GETSET (FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL)
      ILX=(ILL-1)/2
      ILY=MOD(ILL-1,2)
C
C Re-call SET as needed for LPR.
C
      CALL SET    (FFL,FFR,FFB,FFT,FFL,FFR,FFB,FFT,  1)
C
C If the printing of timing information is turned on, initialize the
C two elapsed-time cells.
C
C
C Transform the coordinates of the first point of the polyline and put
C them in the output coordinate arrays.
C
      ICD=1
      ASSIGN 10002 TO L10003
      GO TO 10003
10002 CONTINUE
C
      XCS(1)=XCN
      YCS(1)=YCN
      NCS=1
C
C Loop through the rest of the points of the polyline.  At each point,
C check for intersections of the line segment ending at that point with
C edges in the area map and take the appropriate actions.
C
        ICD = 2
        GO TO 10006
10004   CONTINUE
        ICD =ICD +1
10006   CONTINUE
        IF (ICD .GT.(NCD)) GO TO 10005
C
        IXO=IXN
        IYO=IYN
        XCO=XCN
        YCO=YCN
C
        ASSIGN 10007 TO L10003
        GO TO 10003
10007   CONTINUE
C
10008   CONTINUE
C
          NIF=0
C
C
          IX1=IXO
          IY1=IYO
          IX2=IXN
          IY2=IYN
          FX1=XCO
          FY1=YCO
          FX2=XCN
          FY2=YCN
C
          X21=SIGN(.1,FX2-FX1)
          Y21=SIGN(.1,FY2-FY1)
C
          IXL=MIN(IXO,IXN)
          IXR=MAX(IXO,IXN)+1
C
10009     CONTINUE
            IF (.NOT.(IAM(IPX+1)+IAM(2).LE.IXL)) GO TO 10010
              IPX=IAM(IPX+5)
            GO TO 10011
10010       CONTINUE
            IF (.NOT.(IAM(IAM(IPX+6)+1)+IAM(2).GT.IXL)) GO TO 10012
              IPX=IAM(IPX+6)
            GO TO 10011
10012       CONTINUE
              GO TO 10013
10011       CONTINUE
          GO TO 10009
10013     CONTINUE
C
10014     CONTINUE
          IF (.NOT.(IAM(IPX+1).LT.IXR)) GO TO 10015
            IF (.NOT.(IAM(IPX+7).GT.0.AND.(IAM(IAM(IPX+4)+1).GT.IAM(IPX+
     +1).OR.(IAM(IAM(IPX+4)+1).EQ.IAM(IPX+1).AND.IAM(IAM(IPX+4)+2).GT.IA
     +M(IPX+2))).AND.IAM(IAM(IPX+4)+1).GT.IXL)) GO TO 10016
              IX3=IAM(IPX+1)
              IY3=IAM(IPX+2)
              IX4=IAM(IAM(IPX+4)+1)
              IY4=IAM(IAM(IPX+4)+2)
              FX3=REAL(IX3)
              FY3=REAL(IY3)
              FX4=REAL(IX4)
              FY4=REAL(IY4)
              IF (.NOT.(ABS(IX1+IX2-IX3-IX4).LE.ABS(IX1-IX2)+ABS(IX3-IX4
     +).AND.ABS(IY1+IY2-IY3-IY4).LE.ABS(IY1-IY2)+ABS(IY3-IY4))) GO TO 10
     +017
                ASSIGN 10018 TO L10019
                GO TO 10019
10018           CONTINUE
10017         CONTINUE
10016       CONTINUE
            IF (.NOT.(IAM(IAM(IPX+3)+7).GT.0.AND.(IAM(IAM(IPX+3)+1).GT.I
     +AM(IPX+1).OR.(IAM(IAM(IPX+3)+1).EQ.IAM(IPX+1).AND.IAM(IAM(IPX+3)+2
     +).GT.IAM(IPX+2))).AND.IAM(IAM(IPX+3)+1).GT.IXL)) GO TO 10020
              IX3=IAM(IPX+1)
              IY3=IAM(IPX+2)
              IX4=IAM(IAM(IPX+3)+1)
              IY4=IAM(IAM(IPX+3)+2)
              FX3=REAL(IX3)
              FY3=REAL(IY3)
              FX4=REAL(IX4)
              FY4=REAL(IY4)
              IF (.NOT.(ABS(IX1+IX2-IX3-IX4).LE.ABS(IX1-IX2)+ABS(IX3-IX4
     +).AND.ABS(IY1+IY2-IY3-IY4).LE.ABS(IY1-IY2)+ABS(IY3-IY4))) GO TO 10
     +021
                ASSIGN 10022 TO L10019
                GO TO 10019
10022           CONTINUE
10021         CONTINUE
10020       CONTINUE
            IPX=IAM(IPX+5)
          GO TO 10014
10015     CONTINUE
C
C
          IF (.NOT.(NCS.EQ.MCS)) GO TO 10023
            ASSIGN 10024 TO L10025
            GO TO 10025
10024       CONTINUE
10023     CONTINUE
C
            IIF = 1
            GO TO 10028
10026       CONTINUE
            IIF =IIF +1
10028       CONTINUE
            IF (IIF .GT.(MIN(NIF,10))) GO TO 10027
            NCS=NCS+1
            XCS(NCS)=XCI(IIF)
            YCS(NCS)=YCI(IIF)
            ASSIGN 10029 TO L10025
            GO TO 10025
10029       CONTINUE
          GO TO 10026
10027     CONTINUE
C
          XCO=XCS(NCS)
          YCO=YCS(NCS)
          IXO=INT(XCO)
          IYO=INT(YCO)
C
        IF (.NOT.(NIF.LE.10)) GO TO 10008
C
        IF (.NOT.(ABS(XCN-XCS(NCS)).GT.1..OR.ABS(YCN-YCS(NCS)).GT.1.))
     +  GO TO 10030
          NCS=NCS+1
          XCS(NCS)=XCN
          YCS(NCS)=YCN
10030   CONTINUE
C
      GO TO 10004
10005 CONTINUE
C
C Dump the remaining polyline fragment, if any.
C
      ASSIGN 10031 TO L10025
      GO TO 10025
10031 CONTINUE
C
C Done.
C
C
      GO TO 10033
C
C The following internal procedure computes the X and Y coordinates of
C the next point of the polyline, in the internal coordinate system used
C in the area map.
C
10003 CONTINUE
C
        IF (.NOT.(ILX.EQ.0)) GO TO 10034
          IXN=NINT(MAX(1.,MIN(RLM,
     +                 RLC*(FFL+(FFR-FFL)*(XCD(ICD)-FUL)/(FUR-FUL)))))
        GO TO 10035
10034   CONTINUE
          IXN=NINT(MAX(1.,MIN(RLM,
     +                  RLC*(FFL+(FFR-FFL)*(ALOG(XCD(ICD))-ALOG(FUL))/
     +                                       (ALOG(FUR)-ALOG(FUL))))))
10035   CONTINUE
        IF (.NOT.(ILY.EQ.0)) GO TO 10036
          IYN=NINT(MAX(1.,MIN(RLM,
     +                 RLC*(FFB+(FFT-FFB)*(YCD(ICD)-FUB)/(FUT-FUB)))))
        GO TO 10037
10036   CONTINUE
          IYN=NINT(MAX(1.,MIN(RLM,
     +                  RLC*(FFB+(FFT-FFB)*(ALOG(YCD(ICD))-ALOG(FUB))/
     +                                       (ALOG(FUT)-ALOG(FUB))))))
10037   CONTINUE
C
        XCN=REAL(IXN)
        YCN=REAL(IYN)
C
      GO TO L10003 , (10007,10002)
C
C The following internal procedure checks for intersection of the line
C joining (FX1,FY1) and (FX2,FY2) with the line joining (FX3,FY3) and
C (FX4,FY4).  For each such point of intersection found, an entry is
C made in a stack.
C
10019 CONTINUE
C
        X43=SIGN(.1,FX4-FX3)
        Y43=SIGN(.1,FY4-FY3)
C
        IF (.NOT.(IAU.EQ.1)) GO TO 10038
          TMP=(FX2-FX1)*(FY4-FY3)-(FX4-FX3)*(FY2-FY1)
        GO TO 10039
10038   CONTINUE
        IF (.NOT.(IAU.EQ.2)) GO TO 10040
          DPT=DBLE(IX2-IX1)*DBLE(IY4-IY3)-
     +        DBLE(IX4-IX3)*DBLE(IY2-IY1)
          TMP=REAL(DPT)
        GO TO 10039
10040   CONTINUE
          IO1(3, 1)=IX2-IX1
          IO1(3, 2)=IY4-IY3
          IO1(3, 3)=IX4-IX3
          IO1(3, 4)=IY2-IY1
          CALL ARMPIA (IO1,DPT)
          TMP=REAL(DPT)
10039   CONTINUE
C
        IF (.NOT.(TMP.NE.0.)) GO TO 10041
C
          IF (.NOT.(IAU.EQ.1)) GO TO 10042
            FX0=((FX4-FX3)*(FX2*FY1-FX1*FY2)
     +          -(FX2-FX1)*(FX4*FY3-FX3*FY4))/TMP
          GO TO 10043
10042     CONTINUE
          IF (.NOT.(IAU.EQ.2)) GO TO 10044
            FX0=REAL((DBLE(IX4-IX3)*
     +               (DBLE(IX2)*DBLE(IY1)-DBLE(IX1)*DBLE(IY2))
     +               -DBLE(IX2-IX1)*
     +               (DBLE(IX4)*DBLE(IY3)-DBLE(IX3)*DBLE(IY4)))/DPT)
          GO TO 10043
10044     CONTINUE
            IO2(3, 1)=IX2
            IO2(3, 2)=IY1
            IO2(3, 4)=IX1
            IO2(3, 5)=IY2
            IO2(3, 8)=IX4
            IO2(3, 9)=IY3
            IO2(3,11)=IX3
            IO2(3,12)=IY4
            CALL ARMPIA (IO2,DX0)
            FX0=REAL(DX0/DPT)
10043     CONTINUE
C
          IF (.NOT.(IAU.EQ.1)) GO TO 10045
            FY0=((FY4-FY3)*(FX2*FY1-FX1*FY2)
     +          -(FY2-FY1)*(FX4*FY3-FX3*FY4))/TMP
          GO TO 10046
10045     CONTINUE
          IF (.NOT.(IAU.EQ.2)) GO TO 10047
            FY0=REAL((DBLE(IY4-IY3)*
     +               (DBLE(IX2)*DBLE(IY1)-DBLE(IX1)*DBLE(IY2))
     +               -DBLE(IY2-IY1)*
     +               (DBLE(IX4)*DBLE(IY3)-DBLE(IX3)*DBLE(IY4)))/DPT)
          GO TO 10046
10047     CONTINUE
            CALL ARMPIA (IO3,DY0)
            FY0=REAL(DY0/DPT)
10046     CONTINUE
C
          IF (.NOT.((FX0-FX1+X21)*(FX0-FX2-X21).LT.0..AND.(FY0-FY1+Y21)*
     +(FY0-FY2-Y21).LT.0..AND.(FX0-FX3+X43)*(FX0-FX4-X43).LT.0..AND.(FY0
     +-FY3+Y43)*(FY0-FY4-Y43).LT.0.)) GO TO 10048
C
            DS0=ABS(FX0-FX1)+ABS(FY0-FY1)
C
              IIF = 1
              GO TO 10051
10049         CONTINUE
              IIF =IIF +1
10051         CONTINUE
              IF (IIF .GT.(MIN(NIF,10))) GO TO 10050
              IF (ABS(DS0-DSI(IIF)).LT.1.) GO TO 102
              IF (.NOT.(DS0.LT.DSI(IIF))) GO TO 10052
                IOF=IIF
                GO TO 101
10052         CONTINUE
            GO TO 10049
10050       CONTINUE
C
            IOF=NIF+1
C
  101       CONTINUE
            IF (.NOT.(IOF.LE.10)) GO TO 10053
                IIF = MIN(NIF,9)
                GO TO 10056
10054           CONTINUE
                IIF =IIF -1
10056           CONTINUE
                IF (IIF .LT.(IOF)) GO TO 10055
                XCI(IIF+1)=XCI(IIF)
                YCI(IIF+1)=YCI(IIF)
                DSI(IIF+1)=DSI(IIF)
              GO TO 10054
10055         CONTINUE
              XCI(IOF)=FX0
              YCI(IOF)=FY0
              DSI(IOF)=DS0
10053       CONTINUE
C
            NIF=NIF+1
C
10048     CONTINUE
C
10041   CONTINUE
C
  102   CONTINUE
C
      GO TO L10019 , (10022,10018)
C
C The following internal procedure dumps the current contents of the
C polyline output arrays and prepares them to continue receiving points.
C
10025 CONTINUE
C
        IF (.NOT.(NCS.GT.1)) GO TO 10057
C
          XCP=REAL(INT(.5*(XCS(NCS/2)+XCS(NCS/2+1))))+.5
          YCP=REAL(INT(.5*(YCS(NCS/2)+YCS(NCS/2+1))))+.5
C
          ASSIGN 10058 TO L10059
          GO TO 10059
10058     CONTINUE
C
          XSV=XCS(NCS)
          YSV=YCS(NCS)
C
          DO 10060 I=1,NCS
            XCS(I)=XCS(I)/RLC
            YCS(I)=YCS(I)/RLC
10060     CONTINUE
C
          IF (.NOT.(NAI.EQ.IAM(7))) GO TO 10061
            CALL LPR (XCS,YCS,NCS,IAI,IAG,NAI)
          GO TO 10062
10061     CONTINUE
            CALL SETER ('ARDRLN - ALGORITHM FAILURE',2,1)
            GO TO 10033
10062     CONTINUE
C
          XCS(1)=XSV
          YCS(1)=YSV
          NCS=1
C
10057   CONTINUE
C
      GO TO L10025 , (10031,10029,10024)
C
C The following internal procedure picks up area identifier and group
C identifier information for the point (XCP,YCP) and puts it into the
C user's arrays.
C
10059 CONTINUE
C
C
        IXP=INT(XCP)
C
        NAI=0
C
10064   CONTINUE
          IF (.NOT.(IAM(IPX+1)+IAM(2).LE.IXP)) GO TO 10065
            IPX=IAM(IPX+5)
          GO TO 10066
10065     CONTINUE
          IF (.NOT.(IAM(IAM(IPX+6)+1)+IAM(2).GT.IXP)) GO TO 10067
            IPX=IAM(IPX+6)
          GO TO 10066
10067     CONTINUE
            GO TO 10068
10066     CONTINUE
        GO TO 10064
10068   CONTINUE
C
        IGI=LAM
C
10069   CONTINUE
        IF (.NOT.(IGI.GT.IAM(6))) GO TO 10070
          IGI=IGI-1
          IF (.NOT.(MOD(IAM(IGI),2).EQ.0)) GO TO 10071
            IAF=0
            YCM=RLP
            IPT=IPX
10072       CONTINUE
            IF (.NOT.(IAM(IPT+1).LE.IXP)) GO TO 10073
              IF (.NOT.(ABS(IAM(IPT+7)).EQ.IGI.AND.IAM(IAM(IPT+4)+1).GT.
     +IXP))   GO TO 10074
                IF (.NOT.(IAU.EQ.1)) GO TO 10075
                  YTM=REAL(IAM(IPT+2))+
     +            (XCP-REAL(IAM(IPT+1)))*
     +         (REAL(IAM(IAM(IPT+4)+2)-IAM(IPT+2))/
     + REAL(IAM(IAM(IPT+4)+1)-IAM(IPT+1)))
                GO TO 10076
10075           CONTINUE
                  YTM=REAL(DBLE(IAM(IPT+2))+
     +            (DBLE(XCP)-DBLE(IAM(IPT+1)))*
     +         (DBLE(IAM(IAM(IPT+4)+2)-IAM(IPT+2))/
     + DBLE(IAM(IAM(IPT+4)+1)-IAM(IPT+1))))
10076           CONTINUE
                IF (.NOT.(YTM.GE.YCP.AND.YTM.LT.YCM)) GO TO 10077
                  IAF=IPT+8
                  YCM=YTM
10077           CONTINUE
10074         CONTINUE
              IF (.NOT.(ABS(IAM(IAM(IPT+3)+7)).EQ.IGI.AND.IAM(IAM(IPT+3)
     ++1).GT.IXP)) GO TO 10078
                IF (.NOT.(IAU.EQ.1)) GO TO 10079
                  YTM=REAL(IAM(IPT+2))+
     +            (XCP-REAL(IAM(IPT+1)))*
     +         (REAL(IAM(IAM(IPT+3)+2)-IAM(IPT+2))/
     + REAL(IAM(IAM(IPT+3)+1)-IAM(IPT+1)))
                GO TO 10080
10079           CONTINUE
                  YTM=REAL(DBLE(IAM(IPT+2))+
     +            (DBLE(XCP)-DBLE(IAM(IPT+1)))*
     +         (DBLE(IAM(IAM(IPT+3)+2)-IAM(IPT+2))/
     + DBLE(IAM(IAM(IPT+3)+1)-IAM(IPT+1))))
10080           CONTINUE
                IF (.NOT.(YTM.GE.YCP.AND.YTM.LT.YCM)) GO TO 10081
                  IAF=IAM(IPT+3)+9
                  YCM=YTM
10081           CONTINUE
10078         CONTINUE
              IPT=IAM(IPT+5)
            GO TO 10072
10073       CONTINUE
C
            IF (.NOT.(IAF.NE.0)) GO TO 10082
              ITI=IAM(IAF)
              IF (ITI.GE.IAM(6)) ITI=IAM(ITI)/2
            GO TO 10083
10082       CONTINUE
              ITI=-1
10083       CONTINUE
C
            IF (.NOT.(NAI.LT.MAI)) GO TO 10084
              NAI=NAI+1
              IAI(NAI)=ITI
              IAG(NAI)=IAM(IGI)/2
            GO TO 10085
10084       CONTINUE
              CALL SETER ('ARDRLN - MAI TOO SMALL',3,1)
              GO TO 10033
10085       CONTINUE
C
10071     CONTINUE
C
        GO TO 10069
10070   CONTINUE
C
C
      GO TO L10059 , (10058)
C
C The following internal procedure restores some parameters and returns
C control to the caller.
C
10033 CONTINUE
C
C Restore the new value of IPX to the area map.
C
        IAM(3)=IPX
C
C Restore the original SET call.
C
        CALL SET (FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL)
C
C Return.
C
        RETURN
C
C
      END
