C
C $Id: armvam.f,v 1.1 1993-09-23 17:25:11 kennison Exp $
C
      SUBROUTINE ARMVAM (IAM,IAN,LAN)
C
      DIMENSION IAM(*),IAN(*)
C
C Move an area map from one integer array (IAM) to another (IAN).  The
C length of the first area map can be inferred from its contents; the
C length of the second one is given by the value of the argument LAN.
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
        CALL SETER ('ARMVAM - INITIALIZATION DONE IMPROPERLY',1,1)
        RETURN
10001 CONTINUE
C
C See if the new array is too small.
C
      IF (.NOT.(LAN.LT.LAM-(IAM(6)-IAM(5)-1))) GO TO 10002
        CALL SETER ('ARMVAM - NEW AREA-MAP ARRAY IS TOO SMALL',2,1)
        RETURN
10002 CONTINUE
C
C Move the part of the area map stored at the beginning of the array.
C
      DO 10003 IPT=1,IAM(5)
        IAN(IPT)=IAM(IPT)
10003 CONTINUE
C
C Move the part of the area map stored at the end of the array, taking
C into account the possibility that the arrays may start at the same
C place in memory and we need to avoid overstoring an element that will
C be needed later.
C
      IF (.NOT.(LAN.LT.LAM)) GO TO 10004
        DO 10005 IPT=IAM(6),LAM
          IAN(IPT-LAM+LAN)=IAM(IPT)
10005   CONTINUE
      GO TO 10006
10004 CONTINUE
        DO 10007 IPT=LAM,IAM(6),-1
          IAN(IPT-LAM+LAN)=IAM(IPT)
10007   CONTINUE
10006 CONTINUE
C
C Adjust the three elements (other than those in the point nodes) whose
C values change when the length of the array changes.
C
      IAN(1)=LAN
      IAN(6)=IAN(6)-LAM+LAN
      IAN(LAN)=LAN
C
C Adjust the values of pointers (in the point nodes) that are indices
C of stuff at the end of the array.
C
      ITM=IAN(6)+LAM-LAN
C
      DO 10008 IPT=8,IAN(5)-9,10
        IF (ABS(IAN(IPT+7)).GE.ITM) IAN(IPT+7)=                     SIGN
     +(ABS(IAN(IPT+7))-LAM+LAN,IAN(IPT+7))
        IF (IAN(IPT+8).GT.0) IAN(IPT+8)=IAN(IPT+8)-LAM+LAN
        IF (IAN(IPT+9).GT.0) IAN(IPT+9)=IAN(IPT+9)-LAM+LAN
10008 CONTINUE
C
C Done.
C
      RETURN
C
      END
