C
C $Id: nerro.f,v 1.3 1994-03-16 00:42:49 kennison Exp $
C
      INTEGER FUNCTION NERRO (NERRF)
C
C This function returns the current value of the error flag.
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
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C Return the error flag as the value of both NERRO and NERRF.
C
        NERRO=IERRF
        NERRF=IERRF
C
C Done.
C
        RETURN
C
      END
