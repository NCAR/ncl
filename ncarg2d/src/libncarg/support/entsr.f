C
C $Id: entsr.f,v 1.7 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ENTSR (IROLD,IRNEW)
C
C This routine returns, in IROLD, the current value of the recovery
C flag.  If the value of IRNEW is non-zero, ENTSR also resets the
C recovery flag: IRNEW = 1 activates recovery; IRNEW = 2 deactivates
C recovery.
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDAX.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C Check for an illegal value of IRNEW.
C
        IF (IRNEW.LT.0.OR.IRNEW.GT.2)
     +     CALL SETER ('ENTSR - ILLEGAL VALUE OF THE RECOVERY FLAG',1,2)
C
C Return the previous value of IRECF in IROLD and, if IRNEW is nonzero,
C reset IRECF to that value.
C
        IROLD=IRECF
        IF (IRNEW.NE.0) IRECF=IRNEW
C
C Check for an uncleared prior error that is now unrecoverable.
C
        IF (IERRF.NE.0.AND.IRECF.EQ.2)
     +       CALL SETER ('ENTSR - PRIOR ERROR IS NOW UNRECOVERABLE',2,2)
C
C Done.
C
        RETURN
C
      END
