C
C	$Id: opngks.f,v 1.1.1.1 1992-04-17 22:32:31 ncargd Exp $
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
