C
C $Id: seblda.f,v 1.2 1994-03-16 00:42:53 kennison Exp $
C
      BLOCK DATA SEBLDA
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
C IERRU is the logical unit for error messages.  Its default value is
C zero, which serves as a signal to SETER that the proper value should
C be gotten from I1MACH.
C
        DATA IERRU / 0 /
C
C IERRF is the error flag.  Initially, it is zero.  A non-zero value
C means that there is an uncleared prior error.
C
        DATA IERRF / 0 /
C
C IRECF is the recovery flag, which can have values of 1, which turns
C recovery mode on, or 2, which turns recovery mode off and causes all
C errors to be treated as fatal errors.  The latter is the default.
C
        DATA IRECF / 2 /
C
C LOMSG is the actual length of the error message in ERMSG.
C
        DATA LOMSG / 1 /
C
C ERMSG is the text of the current error message, only meaningful when
C IERRF is non-zero.
C
        DATA ERMSG / ' ' /
C
      END
