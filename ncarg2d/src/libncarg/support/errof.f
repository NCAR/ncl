C
C $Id: errof.f,v 1.3 1994-03-16 00:42:42 kennison Exp $
C
      SUBROUTINE ERROF
C
C This routine just turns off the error flag.
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
C Do it.
C
        IERRF=0
C
C Done.
C
        RETURN
C
      END
