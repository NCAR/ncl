C
C $Id: retsr.f,v 1.2 1993-09-23 17:21:32 kennison Exp $
C
      SUBROUTINE RETSR (IROLD)
C
C This routine restores a saved value of the recovery flag.  If there
C is an active error state and recovery is deactivated, the error
C message is printed and execution stops.
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDA.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*113 ERMSG
        SAVE   /SECOMC/
C
C If the given value of IROLD is illegal, that's an error.
C
        IF (IROLD.LT.1.OR.IROLD.GT.2)
     +         CALL SETER ('RETSR - ILLEGAL VALUE OF RECOVERY FLAG',1,2)
C
        IRECF=IROLD
C
C Check for an uncleared prior error that is now unrecoverable.
C
        IF (IERRF.NE.0.AND.IRECF.EQ.2)
     +       CALL SETER ('RETSR - PRIOR ERROR IS NOW UNRECOVERABLE',2,2)
C
C Done.
C
        RETURN
C
      END
