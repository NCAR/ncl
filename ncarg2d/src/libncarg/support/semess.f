C
C $Id: semess.f,v 1.1 1993-09-23 17:21:36 kennison Exp $
C
      FUNCTION SEMESS ()
C
        CHARACTER*113 SEMESS
C
C The value of this function is the current error message (blanks if
C there is no current error).
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
C Return the message.
C
        IF (IERRF.NE.0) THEN
          SEMESS=ERMSG
        ELSE
          SEMESS=' '
        END IF
C
C Done.
C
        RETURN
C
      END
