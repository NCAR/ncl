C
C $Id: clsgks.f,v 1.2 1993-12-12 20:54:50 kennison Exp $
C
      SUBROUTINE CLSGKS
C
C IU(6), in IUTLCM, is the current metacode unit number, negated if the
C unit is currently in use.
C
      COMMON /IUTLCM/ IU(100)
      SAVE /IUTLCM/
C
C Deactivate the metacode workstation, close the workstation, and
C close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
C Set IU(6) to indicate that the unit is no longer in use.
C
      IU(6)=IABS(IU(6))
C
      RETURN
C
      END
