C
C $Id: arinam.f,v 1.9 1995-04-28 19:41:03 kennison Exp $
C
      SUBROUTINE ARINAM (IAM,LAM)
C
      DIMENSION IAM(LAM)
C
C The routine ARINAM is called to initialize a given area map.  It must
C be called prior to any AREDAM call for that area map.  Input arguments
C are as follows:
C
C IAM is the integer array in which an area map is to be initialized.
C
C LAM is the length of the array IAM.
C
C On output, elements of IAM have been changed, but LAM is unchanged.
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
      IF (ICFELL('ARINAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If AREAS itself has not been initialized, do it now.
C
      IF (.NOT.(IAU.EQ.0)) GO TO 10001
        CALL ARINIT (IER)
        IF (.NOT.(IER.NE.0)) GO TO 10002
          CALL SETER
     +    ('ARINAM/ARINIT - VALUE OF ''LC'' IS TOO LARGE',2,1)
          RETURN
10002   CONTINUE
10001 CONTINUE
C
C Log an error if the user's array is too small.
C
      IF (.NOT.(LAM.LE.27)) GO TO 10003
        CALL SETER ('ARINAM - AREA-MAP ARRAY IS TOO SMALL',3,1)
        RETURN
10003 CONTINUE
C
C Proceed with initialization.  Store the length of the array as its
C first element and as its last; this allows for later error checking.
C
      IAM(1)=LAM
      IAM(LAM)=LAM
C
C Zero the maximum-distance parameter.
C
      IAM(2)=0
C
C Initialize the value of IPX, which preserves from call to call the
C approximate position of the last node with which we did anything.
C
      IAM(3)=8
C
C Initialize the map state.
C
      IAM(4)=0
C
C Set the pointers indicating the locations of the last cells used at
C the beginning and end of the area-map array.
C
      IAM(5)=27
      IAM(6)=LAM
C
C Zero the number of groups.
C
      IAM(7)=0
C
C Set up two dummy nodes to serve as "anchors", preventing searches
C from going past them.
C
      IAM(8)=0
      IAM(9)=-1
      IAM(10)=-1
      IAM(11)=18
      IAM(12)=0
      IAM(13)=18
      IAM(14)=0
      IAM(15)=0
      IAM(16)=0
      IAM(17)=0
C
      IAM(18)=0
      IAM(19)=ILP
      IAM(20)=ILP
      IAM(21)=0
      IAM(22)=8
      IAM(23)=0
      IAM(24)=8
      IAM(25)=0
      IAM(26)=0
      IAM(27)=0
C
C Done.
C
      RETURN
C
      END
