C
C $Id: argtai.f,v 1.4 1993-11-23 18:14:21 kennison Exp $
C
      SUBROUTINE ARGTAI (IAM,XCD,YCD,IAI,IAG,MAI,NAI,ICF)
C
      DIMENSION IAM(*),IAI(*),IAG(*)
C
C The routine ARGTAI is called to obtain information about a specific
C point in an existing area map created by calls to ARINAM and AREDAM.
C
C IAM is the area-map array.
C
C XCD and YCD are the coordinates, in the current user coordinate
C system, of the point at which information is desired.
C
C The arrays IAG and IAI, each of which is dimensioned MAI, are used to
C return information to the caller.  For each I from 1 to NAI, IAI(I)
C will be the area identifier associated with group identifier IAG(I).
C
C ICF is a flag set non-zero to indicate that GETSET should be called
C to get the information necessary to do the mapping from the current
C user coordinate system to the internal coordinate system; if ICF is
C zero, no such calls are done and it is assumed that the information
C saved from a previous call is still correct.
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
C Declare a temporary variable to hold error messages and the SETER
C function that returns the current error message.
C
      CHARACTER*80 MSG
C
      CHARACTER*113 SEMESS
C
C Force values retrieved by the calls to GETSET to be saved from call
C to call.
C
      SAVE FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL,ILX,ILY
C
C Initialize the value of ILL so as to force calls to GETSET on the
C first call ever, no matter what the user says.
C
      DATA ILL / 0 /
C
C Pull out the length of the area map and check for initialization.
C
      LAM=IAM(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAM(LAM).NE.LAM)) GO TO 10001
        CALL SETER ('ARGTAI - INITIALIZATION DONE IMPROPERLY',1,1)
        RETURN
10001 CONTINUE
C
C If it has not already been done, find points of intersection and
C incorporate them into the map and then adjust area identifiers.
C
      IF (.NOT.(IAM(4).EQ.0)) GO TO 10002
        CALL ENTSR (IRO,1)
        CALL ARPRAM (IAM,0,0,0)
        IF (.NOT.(NERRO(IER).NE.0)) GO TO 10003
          MSG(1:7)='ARGTAI/'
          MSG(8:80)=SEMESS()
          CALL ERROF
          CALL RETSR (IRO)
          CALL SETER (MSG,2,1)
          RETURN
10003   CONTINUE
          CALL RETSR (IRO)
10002 CONTINUE
C
C Pull out the current value of the pointer IPX.
C
      IPX=IAM(3)
C
C Use GETSET to set up parameters allowing us to map x and y coordinates
C from the user system to the local integer system.
C
      IF (.NOT.(ICF.NE.0.OR.ILL.EQ.0)) GO TO 10004
        CALL GETSET (FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL)
        ILX=(ILL-1)/2
        ILY=MOD(ILL-1,2)
10004 CONTINUE
C
C Convert the X and Y coordinates to values in the internal coordinate
C range.
C
      IF (.NOT.(ILX.EQ.0)) GO TO 10005
        XCO=NINT(MAX(1.,MIN(RLM,
     +                      RLC*(FFL+(FFR-FFL)*(XCD-FUL)/(FUR-FUL)))))
      GO TO 10006
10005 CONTINUE
        XCO=NINT(MAX(1.,MIN(RLM,
     +                      RLC*(FFL+(FFR-FFL)*(ALOG(XCD)-ALOG(FUL))/
     +                                       (ALOG(FUR)-ALOG(FUL))))))
10006 CONTINUE
      IF (.NOT.(ILY.EQ.0)) GO TO 10007
        YCO=NINT(MAX(1.,MIN(RLM,
     +                      RLC*(FFB+(FFT-FFB)*(YCD-FUB)/(FUT-FUB)))))
      GO TO 10008
10007 CONTINUE
        YCO=NINT(MAX(1.,MIN(RLM,
     +                      RLC*(FFB+(FFT-FFB)*(ALOG(YCD)-ALOG(FUB))/
     +                                       (ALOG(FUT)-ALOG(FUB))))))
10008 CONTINUE
C
C Adjust the X coordinate to keep it away from any integral value and
C compute the integer coordinate which is just to the left of it.
C
      IXO=INT(XCO)
      XCO=REAL(IXO)+.5
C
C Retrieve the desired information from the area map.
C
      NAI=0
C
10009 CONTINUE
        IF (.NOT.(IAM(IPX+1).LE.IXO-IAM(2))) GO TO 10010
          IPX=IAM(IPX+5)
        GO TO 10011
10010   CONTINUE
        IF (.NOT.(IAM(IAM(IPX+6)+1).GT.IXO-IAM(2))) GO TO 10012
          IPX=IAM(IPX+6)
        GO TO 10011
10012   CONTINUE
          GO TO 10013
10011   CONTINUE
      GO TO 10009
10013 CONTINUE
C
      IGI=LAM
C
10014 CONTINUE
      IF (.NOT.(IGI.GT.IAM(6))) GO TO 10015
        IGI=IGI-1
        IF (.NOT.(MOD(IAM(IGI),2).EQ.0)) GO TO 10016
          IAF=0
          YCI=RLP
          IPT=IPX
10017     CONTINUE
          IF (.NOT.(IAM(IPT+1).LE.IXO)) GO TO 10018
            IF (.NOT.(ABS(IAM(IPT+7)).EQ.IGI.AND.IAM(IAM(IPT+4)+1).GT.IX
     +O))   GO TO 10019
              IF (.NOT.(IAU.EQ.1)) GO TO 10020
                YTM=REAL(IAM(IPT+2))+
     +          (XCO-REAL(IAM(IPT+1)))*
     +       (REAL(IAM(IAM(IPT+4)+2)-IAM(IPT+2))/                      R
     +EAL(IAM(IAM(IPT+4)+1)-IAM(IPT+1)))
              GO TO 10021
10020         CONTINUE
                YTM=REAL(DBLE(IAM(IPT+2))+
     +          (DBLE(XCO)-DBLE(IAM(IPT+1)))*
     +       (DBLE(IAM(IAM(IPT+4)+2)-IAM(IPT+2))/                      D
     +BLE(IAM(IAM(IPT+4)+1)-IAM(IPT+1))))
10021         CONTINUE
              IF (.NOT.(YTM.GE.YCO.AND.YTM.LT.YCI)) GO TO 10022
                IAF=IPT+8
                YCI=YTM
10022         CONTINUE
10019       CONTINUE
            IF (.NOT.(ABS(IAM(IAM(IPT+3)+7)).EQ.IGI.AND.IAM(IAM(IPT+3)+1
     +).GT.IXO)) GO TO 10023
              IF (.NOT.(IAU.EQ.1)) GO TO 10024
                YTM=REAL(IAM(IPT+2))+
     +          (XCO-REAL(IAM(IPT+1)))*
     +       (REAL(IAM(IAM(IPT+3)+2)-IAM(IPT+2))/                      R
     +EAL(IAM(IAM(IPT+3)+1)-IAM(IPT+1)))
              GO TO 10025
10024         CONTINUE
                YTM=REAL(DBLE(IAM(IPT+2))+
     +          (DBLE(XCO)-DBLE(IAM(IPT+1)))*
     +       (DBLE(IAM(IAM(IPT+3)+2)-IAM(IPT+2))/                      D
     +BLE(IAM(IAM(IPT+3)+1)-IAM(IPT+1))))
10025         CONTINUE
              IF (.NOT.(YTM.GE.YCO.AND.YTM.LT.YCI)) GO TO 10026
                IAF=IAM(IPT+3)+9
                YCI=YTM
10026         CONTINUE
10023       CONTINUE
            IPT=IAM(IPT+5)
          GO TO 10017
10018     CONTINUE
C
          IF (.NOT.(IAF.NE.0)) GO TO 10027
            ITI=IAM(IAF)
            IF (ITI.GE.IAM(6)) ITI=IAM(ITI)/2
          GO TO 10028
10027     CONTINUE
            ITI=-1
10028     CONTINUE
C
          IF (.NOT.(NAI.LT.MAI)) GO TO 10029
            NAI=NAI+1
            IAI(NAI)=ITI
            IAG(NAI)=IAM(IGI)/2
          GO TO 10030
10029     CONTINUE
            CALL SETER ('ARGTAI - MAI TOO SMALL',3,1)
            RETURN
10030     CONTINUE
C
10016   CONTINUE
C
      GO TO 10014
10015 CONTINUE
C
C Restore the new value of IPX to the area map.
C
      IAM(3)=IPX
C
C Check for a bad value of NAI.
C
      IF (.NOT.(NAI.NE.IAM(7))) GO TO 10031
        CALL SETER ('ARGTAI - ALGORITHM FAILURE',4,1)
        RETURN
10031 CONTINUE
C
C Done.
C
      RETURN
C
      END
