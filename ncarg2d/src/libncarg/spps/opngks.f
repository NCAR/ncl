C
C $Id: opngks.f,v 1.3 1994-03-17 01:44:04 kennison Exp $
C
      SUBROUTINE OPNGKS
C
C IU(6), in IUTLCM, is the current metacode unit number, negated if the
C unit is currently in use.
C
      COMMON /IUTLCM/ IU(100)
      SAVE /IUTLCM/
C
C Force the required BLOCKDATA to load.
C
      EXTERNAL UTILBD
C
C Make sure that, if there is an uncleared prior error, the error
C message gets printed.  Go ahead and open GKS, though.
C
      IERR=ICFELL('OPNGKS - UNCLEARED PRIOR ERROR',1)
C
C Open GKS, define a workstation, and activate the workstation.
C
      IF (IU(6).GT.0) THEN
        CALL GOPKS (6,0)
        CALL GOPWK (1,IU(6),1)
        CALL GACWK (1)
        IU(6)=-IABS(IU(6))
      END IF
C
      RETURN
C
      END
